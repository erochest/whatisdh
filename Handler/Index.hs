
module Handler.Index
    ( getIndexR
    , getReindexR
    , postReindexR
    ) where


import           Import
import           Text.Coffee


getIndexR :: Handler RepHtml
getIndexR = do
    defaultLayout $ do
        setTitle "What is DH? Token Index"

        addScript $ StaticR js_jquery_1_7_2_min_js
        addScript $ StaticR js_underscore_min_js
        addScript $ StaticR js_backbone_min_js

        toWidget $(coffeeFile "templates/index.coffee")
        $(widgetFile "index")

getReindexR :: Handler RepHtml
getReindexR = undefined

postReindexR :: Handler RepHtml
postReindexR = undefined

