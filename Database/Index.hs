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

-- This indexes the documents into the TokenType and TokenIndex tables.
indexDocs :: MonadIO m => [Entity Document] -> SqlPersist m ()
indexDocs docs = do
    undefined
    -- TokenType
    -- TokenIndex
    -- Bigram
    -- TokenChain


logLine :: MonadIO m => String -> m ()
logLine msg = liftIO (putStrLn msg >> hFlush stdout)

type IndexKey = (Int, T.Text)

deleteIndex :: forall (b :: (* -> *) -> * -> *) (m :: * -> *). (MonadIO (b m), PersistQuery b m)
            => b m ()
deleteIndex =
    undefined

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

