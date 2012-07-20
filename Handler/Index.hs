{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Index
    ( getIndexR
    , getIndexDataR
    , getReindexR
    , postReindexR
    ) where


import           Control.Applicative
import           Control.Concurrent
import           Control.Exception hiding (Handler)
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import           Data.Ord
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Database.Index (reIndexAll)
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Postgresql
import           Database.Persist.Store
import           Import
import           Text.Coffee
import           Text.Index
import           System.IO

getIndexR :: Handler RepHtml
getIndexR = defaultLayout $ do
    setTitle "What is DH? Token Index"

    addScript $ StaticR js_jquery_1_7_2_min_js
    addScript $ StaticR js_underscore_min_js
    addScript $ StaticR js_json2_js
    addScript $ StaticR js_backbone_min_js
    addScript $ StaticR js_backbone_paginator_min_js

    toWidget $(coffeeFile "templates/index.coffee")
    $(widgetFile "index")

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

trd3 :: (a, b, c) -> c
trd3 (_, _, a) = a

getOrderF :: Maybe T.Text -> ([a] -> [a])
getOrderF (Just "asc")  = id
getOrderF (Just "desc") = L.reverse
getOrderF Nothing       = id

getIndexDataR :: Handler RepJson
getIndexDataR = do
    Pagination moffset mlimit morderby msort <- runInputGet paginationForm

    trigrams' <- (accumTrigrams . map entityVal) <$> (runDB $ selectList [] [])
    let sorter :: (Trigram, Int) -> (Trigram, Int) -> Ordering
        sorter   = case morderby of
                    Just "freq"    -> comparing snd
                    Just "token_1" -> comparing (fst3 . fst)
                    Just "token_2" -> comparing (snd3 . fst)
                    Just "token_3" -> comparing (trd3 . fst)
                    Nothing        -> comparing snd
        order    = getOrderF msort
        offset   = maybe 0   id moffset
        limit    = maybe 100 id mlimit
        trigrams = L.take limit
                 . L.drop offset
                 . order
                 . L.sortBy sorter
                 $ M.toList trigrams'

        tojson :: ((T.Text, T.Text, T.Text), Int) -> Value
        tojson ((t1, t2, t3), fq) = AT.object [ "token_1"   .= t1
                                              , "token_2"   .= t2
                                              , "token_3"   .= t3
                                              , "freq"      .= fq
                                              ]

    jsonToRepJson $ AT.object [ "count"   .= M.size trigrams'
                              , "results" .= array (map tojson trigrams)
                              ]

getReindexR :: Handler RepHtml
getReindexR = defaultLayout $ do
    setTitle "What is DH? Re-index Documents"
    addScript $ StaticR js_jquery_1_7_2_min_js
    toWidget $(coffeeFile "templates/reindex.coffee")
    $(widgetFile "reindex")

postReindexR :: Handler RepJson
postReindexR = do
    config <- persistConfig `fmap` getYesod
    mvar   <- liftIO newEmptyMVar
    -- Change output to Either and include error message.
    _ <- liftIO . forkIO $ do
        (index config >>= putMVar mvar . Just) `onException` (putMVar mvar Nothing)

    output <- liftIO $ takeMVar mvar
    jsonToRepJson $ maybe AT.Null outToJs output

    where
        outToJs (dCount, tCount, bCount, trCount, elapsed) =
            AT.object [ "document_count" .= dCount
                      -- , "token_count"    .= tCount
                      -- , "bigram_count"   .= bCount
                      -- , "trigram_count"  .= trCount
                      , "elapsed_time"   .= show elapsed
                      ]

        index :: PostgresConf -> IO (Int, Int, Int, Int, NominalDiffTime)
        index config = do
            start  <- getCurrentTime
            (dCount, tCount, bCount, trCount) <-
                withPostgresqlConn (pgConnStr config) $ runSqlConn $ do
                    reIndexAll $ Just 1000
                    dc  <- count ([] :: [Filter Document])
                    -- tc  <- count ([] :: [Filter TokenType])
                    -- bc  <- count ([] :: [Filter Bigram])
                    -- trc <- count ([] :: [Filter TokenChain])
                    return (dc, 0, 0, 0)
            end <- getCurrentTime
            return (dCount, tCount, bCount, trCount, end `diffUTCTime` start)
            where log msg = liftIO (putStrLn msg >> hFlush stdout)

