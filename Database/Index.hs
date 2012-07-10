{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Index
    ( indexDocs
    , deleteIndex
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

withTmpObj :: MonadIO m => T.Text -> T.Text -> T.Text -> SqlPersist m a -> SqlPersist m a
withTmpObj objType name createSql action = do
    execSql createSql
    a <- action
    execSql $ mconcat [ "DROP ", objType, " ", name, ";" ]
    return a

withTmpTable :: MonadIO m => T.Text -> T.Text -> SqlPersist m a -> SqlPersist m a
withTmpTable = withTmpObj "TABLE"

withTmpView :: MonadIO m => T.Text -> T.Text -> SqlPersist m a -> SqlPersist m a
withTmpView = withTmpObj "VIEW"

execSeq :: MonadIO m => T.Text -> [a] -> (a -> [PersistValue]) -> SqlPersist m ()
execSeq sql params coerce = mapM_ (execute sql) $ map coerce params

execSql :: MonadIO m => T.Text -> SqlPersist m ()
execSql sql = execute sql []

upInsertUp :: MonadIO m => T.Text -> T.Text -> SqlPersist m ()
upInsertUp updateSql insertSql = mapM_ execSql [ updateSql
                                               , insertSql
                                               , updateSql
                                               ]

-- This indexes the documents into the TokenType, TokenIndex, Bigram, and
-- TokenChain tables.
indexDocs :: MonadIO m => [Entity Document] -> SqlPersist m ()
indexDocs docs = do
    -- populate TokenType
    let createSql = " create temporary table tmp_indexing \
                      ( seq integer not null, \
                        doc_id integer not null, \
                        token_id integer default null, \
                        bigram_id integer default null, \
                        chain_id integer default null, \
                        text varchar not null, \
                        category varchar not null ); "
    liftIO $ putStrLn "creating temporary table"
    withTmpTable "tmp_indexing" createSql $ do
        -- TokenType
        let populate  = " INSERT INTO tmp_indexing \
                          (seq, doc_id, text, category) \
                          VALUES (?, ?, ?, ?) ;"
            updateSql = " UPDATE tmp_indexing \
                          SET token_id=token_type.id \
                          FROM token_type \
                          WHERE tmp_indexing.text=token_type.text AND \
                                tmp_indexing.token_id IS NULL; "
            insertSql = " INSERT INTO token_type (text, token_category) \
                          SELECT DISTINCT tmp.text, tmp.category \
                          FROM tmp_indexing tmp \
                          WHERE token_id IS NULL; "

        liftIO $ putStrLn "populating TokenType"
        execSeq populate (zip ([1..] :: [Int]) tokens) $ \(i, (d, (t, c))) ->
            [ toPersistValue i
            , toPersistValue d
            , toPersistValue t
            , toPersistValue c
            ]
        upInsertUp updateSql insertSql

        -- TokenIndex
        liftIO $ putStrLn "populating TokenIndex"
        let deleteSql = " DELETE FROM token_index \
                          WHERE document_id IN ( \
                          SELECT DISTINCT doc_id FROM tmp_indexing  \
                          ) ; "
            insertSql = " INSERT INTO token_index \
                          (token_id, document_id, freq) \
                          SELECT token_id, doc_id, COUNT(*) \
                          FROM tmp_indexing \
                          GROUP BY token_id, doc_id ; "
        execSql deleteSql
        execSql insertSql

        -- Bigram
        liftIO $ putStrLn "populating Bigram"
        let updateSql = " UPDATE tmp_indexing \
                          SET bigram_id=b.id \
                          FROM bigram b, tmp_indexing t2 \
                          WHERE tmp_indexing.bigram_id IS NULL AND \
                                tmp_indexing.token_id=b.fst_token AND \
                                t2.token_id=b.snd_token AND \
                                tmp_indexing.seq=t2.seq-1 AND \
                                tmp_indexing.doc_id=t2.doc_id \
                                ; "
            insertSql = " INSERT INTO bigram (fst_token, snd_token) \
                          SELECT DISTINCT t1.token_id, t2.token_id \
                          FROM tmp_indexing t1, tmp_indexing t2 \
                          WHERE t1.bigram_id IS NULL AND \
                                t1.seq=t2.seq-1 AND t1.doc_id=t2.doc_id \
                          ; "
        upInsertUp updateSql insertSql

        -- TokenChain
        liftIO $ putStrLn "populating TokenChain"
        let updateSql = " UPDATE tmp_indexing \
                          SET chain_id=c.id \
                          FROM token_chain c, tmp_indexing t3 \
                          WHERE tmp_indexing.chain_id IS NULL AND \
                                tmp_indexing.bigram_id=c.bigram AND \
                                tmp_indexing.seq=t3.seq-2 AND \
                                tmp_indexing.doc_id=t3.doc_id AND \
                                t3.token_id=c.next \
                          ; "
            insertSql = " INSERT INTO token_chain (bigram, next, frequency) \
                          SELECT DISTINCT b.bigram_id, t3.token_id, 0 \
                          FROM tmp_indexing b \
                          JOIN tmp_indexing t3 ON b.doc_id=t3.doc_id AND \
                                                  b.seq=t3.seq-2 \
                          WHERE b.chain_id IS NULL \
                          ; "
            viewSql   = " CREATE TEMPORARY VIEW tmp_freq_view AS \
                          SELECT b.bigram_id AS bid, t3.token_id AS tid, COUNT(*) as freq \
                          FROM tmp_indexing b \
                          JOIN tmp_indexing t3 ON b.doc_id=t3.doc_id AND \
                                                  b.seq=t3.seq-2 \
                          GROUP BY b.bigram_id, t3.token_id \
                          ; "
            freqSql   = " UPDATE token_chain \
                          SET frequency=frequency+freq \
                          FROM tmp_freq_view \
                          WHERE tmp_freq_view.bid=bigram AND \
                                tmp_freq_view.tid=next \
                          ; "
        upInsertUp updateSql insertSql
        withTmpView "tmp_freq_view" viewSql $ execSql freqSql

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

-- deleteIndex :: MonadIO m => SqlPersist m ()
deleteIndex :: forall (b :: (* -> *) -> * -> *) (m :: * -> *). PersistQuery b m
            => b m ()
deleteIndex = do
    deleteWhere ([] :: [Filter TokenChain])
    deleteWhere ([] :: [Filter Bigram])
    deleteWhere ([] :: [Filter TokenIndex])
    deleteWhere ([] :: [Filter TokenType])

