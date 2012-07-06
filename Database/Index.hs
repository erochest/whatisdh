{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Index
    ( indexDocs
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List as L
import           Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Store
import           Import
import           Text.Tokenizer (tokenize)

withTmpTable :: MonadIO m => T.Text -> (T.Text -> SqlPersist m a) -> SqlPersist m a
withTmpTable source action = do
    execute createSql []
    a <- action tmp
    execute dropSql []
    return a
    where
        tmp       = "tmp_" `mappend` source
        createSql = mconcat [ "CREATE TEMPORARY TABLE "
                            , tmp
                            , " AS SELECT * FROM "
                            , " WHERE false;"
                            ]
        dropSql   = mconcat [ "DROP TABLE ", tmp, ";" ]

execSeq :: MonadIO m => T.Text -> [a] -> (a -> [PersistValue]) -> SqlPersist m ()
execSeq sql params coerce = mapM_ (execute sql) $ map coerce params

-- This indexes the documents into the TokenType, TokenIndex, Bigram, and
-- TokenChain tables.
indexDocs :: MonadIO m => [Entity Document] -> SqlPersist m ()
indexDocs docs = do
    -- populate TokenType
    withTmpTable "token_type" $ \tmp -> do
        let populate  = mconcat [ "INSERT INTO ", tmp
                                , " (text, token_category) VALUES (?, ?);"
                                ]
            insertSql = mconcat [ "INSERT INTO token_type (text, token_category) "
                                , "SELECT tmp.text, tmp.token_category "
                                , "FROM ", tmp, " tmp "
                                , "LEFT JOIN token_type t ON t.text=tmp.text "
                                , "WHERE t.id IS NULL;"
                                ]
                
        execSeq populate (M.toList tokenSet) $ \(t, c) ->
            [toPersistValue t, toPersistValue c]
        execute insertSql []

    -- populate TokenIndex
    let docIndexSql = " INSERT INTO token_index \
                        (document_id, token_id, freq) \
                        VALUES (?, ?, ?);"
    execSeq docIndexSql (M.toList dFreqs) $ \((did, t), f) ->
        [toPersistValue did, toPersistValue t, toPersistValue f]

    -- TODO: Instead of populating separate tables, create one temporary table
    -- that has optional ID columns for all the entities and three columns for
    -- token texts. Populate the token texts and then update the IDs from the
    -- original tables. Insert rows with null IDs into the primary tables, and
    -- re-update with the newly created IDs. Then do the same with the bigram
    -- IDs and token chain IDs. This would probably also work for populating
    -- the token types and token index as well.

    -- populate Bigram
    withTmpTable "bigram" $ \tmp -> do
        let populate  = mconcat [ "INSERT INTO ", tmp
                                , " (fst_token, snd_token) VALUES (?, ?);"
                                ]
            insertSql = mconcat [ "INSERT INTO bigram (fst_token, snd_token) "
                                , "SELECT tmp.fst_token, tmp.snd_token "
                                , "FROM ", tmp, " tmp "
                                , "LEFT JOIN bigram b ON b.fst_token=tmp.fst_token "
                                , "AND b.snd_token=tmp.snd_token "
                                , "WHERE b.id IS NULL;"
                                ]
        execSeq populate (S.toList bigramSet) $ \(a, b) ->
            [toPersistValue a, toPersistValue b]
        execute insertSql []

    -- populate TokenChain

    where
        tokens = L.concatMap tokenizeDoc docs

        tokenizeDoc (Entity (Key (PersistInt64 did)) (Document {..})) =
            case tokenize documentContent of
                Left _     -> []
                Right tkns -> map ((,) (fromIntegral did)) tkns
        tokenizeDoc _ = []

        tokenSet = L.foldl' (\m (_, (t, c)) -> M.insert t c m) M.empty tokens

        addFreq :: M.HashMap IndexKey Int
                -> (Int, (T.Text, TokenCategory))
                -> M.HashMap IndexKey Int
        addFreq m (did, (t, _)) = M.insertWith (+) (did, t) 1 m

        dFreqs = L.foldl' addFreq M.empty tokens

        chains = concatMap (triples . map (fst . snd)) $ L.groupBy gb tokens

        gb (a, _) (b, _) = a == b

        bigramSet = L.foldl' (\s (a, b, _) -> S.insert (a, b) s) S.empty chains

        -- redefine in terms of `tails` and `catMaybes`?
        triples :: [a] -> [(a, a, a)]
        triples (a: (as@(b: (c: _)))) = (a, b, c) : triples as
        triples _                     = []

type IndexKey = (Int, T.Text)

-- * start transaction;
-- * create temporary table temporary_table as select * from test where false;
-- * copy temporary_table from 'data_file.csv';
-- lock table test;
-- update test set data=temporary_table.data from temporary_table where test.id=temporary_table.id;
-- insert into test select * from temporary_table where id not in (select id from test) as a

