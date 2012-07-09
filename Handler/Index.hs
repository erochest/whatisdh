{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Index
    ( getIndexR
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
import           Data.Maybe (catMaybes, maybe)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Database.Index (indexDocs)
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Postgresql
import           Database.Persist.Store
import           Import
import           Text.Coffee

getIndexR :: Handler RepHtmlJson
getIndexR = do
    let select = " SELECT tc.id, b.id, t1.text, t2.text, t3.text \
                   FROM token_chain tc \
                   JOIN bigram b ON tc.bigram=b.id \
                   JOIN token_type t1 ON b.fst_token=t1.id \
                   JOIN token_type t2 ON b.snd_token=t2.id \
                   JOIN token_type t3 ON tc.next=t3.id \
                   "
        showpack = T.pack . show

        totuple :: [PersistValue] -> Maybe (Int, Int, T.Text, T.Text, T.Text)
        totuple [PersistInt64 tcId, PersistInt64 bId, PersistText t1, PersistText t2, PersistText t3] =
            Just (fromIntegral tcId, fromIntegral bId, t1, t2, t3)
        totuple _ = Nothing

        tojson :: (Int, Int, T.Text, T.Text, T.Text) -> Value
        tojson (tcId, bId, t1, t2, t3) = AT.object [ "chain_id"  .= tcId
                                                   , "bigram_id" .= bId
                                                   , "tokens"    .= array [t1, t2, t3]
                                                   ]

    Pagination moffset mlimit morderby msort <- runInputGet paginationForm
    let sql = T.concat $ [select] ++ catMaybes
                [ (mappend " ORDER BY ")          <$> morderby
                , (mappend " ")                   <$> msort
                , (mappend " LIMIT " . showpack)  <$> mlimit
                , (mappend " OFFSET " . showpack) <$> moffset
                , Just ";"
                ]
    $(logDebug) ("SQL: " `mappend` sql)
    results <- runDB . C.runResourceT $     withStmt sql []
                                      C.$= CL.map totuple
                                      C.$= CL.map (fmap tojson)
                                      C.$$ CL.consume
    $(logDebug) ("RESULTS COUNT: " `mappend` showpack (length results))

    let html = do
            setTitle "What is DH? Token Index"

            addScript $ StaticR js_jquery_1_7_2_min_js
            addScript $ StaticR js_underscore_min_js
            addScript $ StaticR js_backbone_min_js
            addScript $ StaticR js_backbone_paginator_min_js

            toWidget $(coffeeFile "templates/index.coffee")
            $(widgetFile "index")

        jsResults = array $ catMaybes results

    defaultLayoutJson html jsResults

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
    liftIO . forkIO $ do
        (index config >>= putMVar mvar . Just) `onException` (putMVar mvar Nothing)

    output <- liftIO $ takeMVar mvar
    jsonToRepJson $ maybe AT.Null outToJs output

    where
        outToJs (dCount, elapsed) =
            AT.object [ "document_count" .= dCount
                      , "token_count"    .= (144 :: Int)
                      , "elapsed_time"   .= show elapsed
                      ]

        index :: PostgresConf -> IO (Int, NominalDiffTime)
        index config = do
            start  <- getCurrentTime
            dCount <- withPostgresqlConn (pgConnStr config) $ runSqlConn $ do
                (docs :: [Entity Document]) <- selectList [] []
                -- indexDocs docs
                return (length docs)
            end <- getCurrentTime
            return (dCount, end `diffUTCTime` start)

