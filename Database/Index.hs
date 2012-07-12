{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Index
    ( indexDocs
    , deleteIndex
    , reIndexAll
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO)
import qualified Data.List as L
import           Data.List.Split (splitEvery)
import           Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Store
import           Import
import           Text.Tokenizer (tokenize)
import           System.IO (stdout, hFlush)

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
    logLine ("indexing " ++ (show (length docs)) ++ " documents")
    let createSql = " create temporary table tmp_indexing \
                      ( seq integer not null, \
                        doc_id integer not null, \
                        token_id integer default null, \
                        text varchar not null, \
                        category varchar not null ); "
        indexSql  = " create index idx_tmp_indexing \
                      on tmp_indexing \
                      (seq, doc_id, token_id, text) \
                      ; "
    logLine ("creating temporary table")
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

        logLine ("populating TokenType")
        execSeq populate (zip ([1..] :: [Int]) tokens) $ \(i, (d, (t, c))) ->
            [ toPersistValue i
            , toPersistValue d
            , toPersistValue t
            , toPersistValue c
            ]

        logLine ("creating temporary index")
        execSql indexSql

        upInsertUp updateSql insertSql

        -- TokenIndex
        logLine "populating TokenIndex"
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

    let createSql = " CREATE TEMPORARY TABLE tmp_chains \
                      ( bigram_id integer default null, \
                        chain_id integer default null, \
                        t1_id integer default null, \
                        t2_id integer default null, \
                        t3_id integer default null, \
                        t1 text, \
                        t2 text, \
                        t3 text ); "
        index1Sql  = " CREATE INDEX idx_tmp_chains_text ON tmp_chains \
                       ( t1, t2, t3 ); "
        index2Sql  = " CREATE INDEX idx_tmp_chains_id ON tmp_chains \
                       ( bigram_id, chain_id, t1_id, t2_id, t3_id ); "
    logLine ("creating temporary table")
    withTmpTable "tmp_chains" createSql $ do
        logLine ("indexing chains")
        execSql index1Sql
        execSql index2Sql

        -- Bigram
        logLine "populating Bigram"
        let insertCSql  = "INSERT INTO tmp_chains (t1, t2, t3) VALUES (?, ?, ?);"
            updateT1Sql = "UPDATE tmp_chains SET t1_id=t.id \
                           FROM token_type t WHERE t.text=t1;"
            updateT2Sql = "UPDATE tmp_chains SET t2_id=t.id \
                           FROM token_type t WHERE t.text=t2;"
            updateT3Sql = "UPDATE tmp_chains SET t3_id=t.id \
                           FROM token_type t WHERE t.text=t3;"
            updateSql = " UPDATE tmp_chains SET bigram_id=b.id \
                          FROM bigram b \
                          WHERE bigram_id IS NULL AND \
                                b.fst_token=t1_id AND b.snd_token=t2_id; "
            insertSql = " INSERT INTO bigram (fst_token, snd_token) \
                          SELECT DISTINCT t.t1_id, t.t2_id \
                          FROM tmp_chains t \
                          WHERE t.bigram_id IS NULL ;"
        execSeq insertCSql chains $ \(t1, t2, t3) ->
            [ toPersistValue t1
            , toPersistValue t2
            , toPersistValue t3
            ]
        mapM_ execSql [ updateT1Sql, updateT2Sql, updateT3Sql ]
        upInsertUp updateSql insertSql

        -- TokenChain
        logLine "populating TokenChain"
        let updateSql = " UPDATE tmp_chains \
                          SET chain_id=c.id \
                          FROM token_chain c \
                          WHERE chain_id IS NULL AND \
                                bigram_id=c.bigram AND \
                                t3_id=c.next \
                          ; "
            insertSql = " INSERT INTO token_chain (bigram, next, frequency) \
                          SELECT DISTINCT c.bigram_id, c.t3_id, 0 \
                          FROM tmp_chains c \
                          WHERE c.chain_id IS NULL \
                          ; "
            viewSql   = " CREATE TEMPORARY VIEW tmp_freq_view AS \
                          SELECT c.bigram_id AS bid, c.t3_id AS tid, COUNT(*) as freq \
                          FROM tmp_chains c \
                          GROUP BY c.bigram_id, c.t3_id \
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

logLine :: MonadIO m => String -> m ()
logLine msg = liftIO (putStrLn msg >> hFlush stdout)

type IndexKey = (Int, T.Text)

deleteIndex :: forall (b :: (* -> *) -> * -> *) (m :: * -> *). (MonadIO (b m), PersistQuery b m)
            => b m ()
deleteIndex = do
    logLine "\tdelete TokenChain"
    deleteWhere ([] :: [Filter TokenChain])
    logLine "\tdelete Bigram"
    deleteWhere ([] :: [Filter Bigram])
    logLine "\tdelete TokenIndex"
    deleteWhere ([] :: [Filter TokenIndex])
    logLine "\tdelete TokenType"
    deleteWhere ([] :: [Filter TokenType])

reIndexAll :: forall (m :: * -> *)
           .  (MonadUnsafeIO m, MonadThrow m, MonadIO m, MonadBaseControl IO m)
           => Maybe Int -> SqlPersist m ()
reIndexAll chunkSize = do
    deleteIndex

    -- It may be more efficient to page through the results at the DB level and
    -- to run each call to indexDocs in its own transaction.
    (docs :: [Entity Document]) <- selectList [] []
    let docChunks = case chunkSize of
            Just cs -> splitEvery cs docs
            Nothing -> [docs]
    mapM_ indexDocs docChunks

