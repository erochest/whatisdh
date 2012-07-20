{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import           Import
import           Text.DissociatedPress
import           Text.Index

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    trigrams <-  (M.keys . accumTrigrams . map entityVal)
             <$> (runDB $ selectList [] [])
    tokens   <- liftIO $ dissociate trigrams
    let press = T.intercalate " " $ L.take 500 tokens
    defaultLayout $ do
        setTitle "What is DH?"
        $(widgetFile "homepage")

