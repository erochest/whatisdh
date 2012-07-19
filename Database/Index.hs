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
import           Data.Maybe
import           Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Store
import           Import
import           System.IO (stdout, hFlush)
import           Text.Index (indexDocument)

updateVal :: entity -> Entity entity -> Entity entity
updateVal e entity = entity { entityVal = e }

modifyEntity :: Functor f
             => (a -> b) -> (b -> a -> a) -> (b -> f b) -> a -> f a
modifyEntity getter setter modifier item =
    (flip setter item) `fmap` (modifier $ getter item)

-- This indexes the documents into the TokenType and TokenIndex tables.
indexDocs :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, MonadBaseControl IO m)
          => [Entity Document] -> SqlPersist m ()
indexDocs docs =
    mapM_ updateTrigrams . catMaybes $ map (modifyEntity entityVal updateVal indexDocument) docs
    where
        updateTrigrams (Entity key doc) =
            update key [ DocumentTrigrams =. (documentTrigrams doc) ]

logLine :: MonadIO m => String -> m ()
logLine msg = liftIO (putStrLn msg >> hFlush stdout)

type IndexKey = (Int, T.Text)

deleteIndex :: forall (b :: (* -> *) -> * -> *) (m :: * -> *). (MonadIO (b m), PersistQuery b m)
            => b m ()
deleteIndex = updateWhere [] [DocumentTrigrams =. Nothing]

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

