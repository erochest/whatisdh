{-# LANGUAGE OverloadedStrings #-}

module Handler.Index
    ( getIndexR
    , getReindexR
    , postReindexR
    ) where


import           Control.Applicative
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Maybe (catMaybes)
import           Data.Monoid
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.GenericSql
import           Database.Persist.GenericSql.Raw
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
    pagination <- runInputGet paginationForm
    results <- case pagination of
        (Pagination moffset mlimit morderby msort) -> do
            let sql = T.concat $ [select] ++ catMaybes
                        [ (mappend " ORDER BY ")               <$> morderby
                        , (mappend " ")                        <$> msort
                        , (mappend " LIMIT " . T.pack . show)  <$> mlimit
                        , (mappend " OFFSET " . T.pack . show) <$> moffset
                        , Just ";"
                        ]
            $(logDebug) ("SQL: " `mappend` sql)
            results <- runDB . C.runResourceT $ withStmt sql [] C.$$ CL.consume
            undefined
        _ -> return []

    let html = do
            setTitle "What is DH? Token Index"

            addScript $ StaticR js_jquery_1_7_2_min_js
            addScript $ StaticR js_underscore_min_js
            addScript $ StaticR js_backbone_min_js
            addScript $ StaticR js_backbone_paginator_min_js

            toWidget $(coffeeFile "templates/index.coffee")
            $(widgetFile "index")

        json = ()

    defaultLayoutJson html json

getReindexR :: Handler RepHtml
getReindexR = undefined

postReindexR :: Handler RepHtml
postReindexR = undefined

