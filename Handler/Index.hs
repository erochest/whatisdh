
module Handler.Index
    ( getIndexR
    , getReindexR
    , postReindexR
    ) where

import           Import

getIndexR :: Handler RepHtml
getIndexR = do
    defaultLayout $ do
        setTitle "What is DH? Token Index"
        $(widgetFile "index")

getReindexR :: Handler RepHtml
getReindexR = undefined

postReindexR :: Handler RepHtml
postReindexR = undefined

