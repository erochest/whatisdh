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
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
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

getIndexDataR :: Handler RepJson
getIndexDataR = do
    Pagination moffset mlimit morderby msort <- runInputGet paginationForm

    let showpack = T.pack . show

        totuple :: [PersistValue] -> Maybe (Int, Int, T.Text, T.Text, T.Text, Int)
        totuple [ PersistInt64 tcId
                , PersistInt64 bId
                , PersistText t1
                , PersistText t2
                , PersistText t3
                , PersistInt64 fq
                ] =
            Just (fromIntegral tcId, fromIntegral bId, t1, t2, t3, fromIntegral fq)
        totuple _ = Nothing

        tojson :: (Int, Int, T.Text, T.Text, T.Text, Int) -> Value
        tojson (tcId, bId, t1, t2, t3, fq) = AT.object [ "id"        .= tcId
                                                       , "bigram_id" .= bId
                                                       , "token_1"   .= t1
                                                       , "token_2"   .= t2
                                                       , "token_3"   .= t3
                                                       , "freq"      .= fq
                                                       ]

        select = " SELECT tc.id, b.id, t1.text token_1, t2.text token_2, t3.text token_3, tc.frequency freq \
                   FROM token_chain tc \
                   JOIN bigram b ON tc.bigram=b.id \
                   JOIN token_type t1 ON b.fst_token=t1.id \
                   JOIN token_type t2 ON b.snd_token=t2.id \
                   JOIN token_type t3 ON tc.next=t3.id \
                   "
        sql = T.concat $ [select] ++ catMaybes
                [ (mappend " ORDER BY ")          <$> morderby
                , (mappend " ")                   <$> msort
                , (mappend " LIMIT " . showpack)  <$> mlimit
                , (mappend " OFFSET " . showpack) <$> moffset
                , Just ";"
                ]

    $(logDebug) ("SQL: " `mappend` sql)
    results <- runDB . C.runResourceT $     withStmt sql []
                                      C.$= CL.map (fmap tojson . totuple)
                                      C.$$ CL.consume
    $(logDebug) ("RESULTS COUNT: " `mappend` showpack (length results))
    chainCount <- runDB $ count ([] :: [Filter TokenChain])

    jsonToRepJson $ AT.object [ "count"   .= chainCount
                              , "results" .= array (catMaybes results)
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
                      , "token_count"    .= tCount
                      , "bigram_count"   .= bCount
                      , "trigram_count"  .= trCount
                      , "elapsed_time"   .= show elapsed
                      ]

        index :: PostgresConf -> IO (Int, Int, Int, Int, NominalDiffTime)
        index config = do
            start  <- getCurrentTime
            (dCount, tCount, bCount, trCount) <-
                withPostgresqlConn (pgConnStr config) $ runSqlConn $ do
                    reIndexAll $ Just 1000
                    dc  <- count ([] :: [Filter Document])
                    tc  <- count ([] :: [Filter TokenType])
                    bc  <- count ([] :: [Filter Bigram])
                    trc <- count ([] :: [Filter TokenChain])
                    return (dc, tc, bc, trc)
            end <- getCurrentTime
            return (dCount, tCount, bCount, trCount, end `diffUTCTime` start)
            where log msg = liftIO (putStrLn msg >> hFlush stdout)

