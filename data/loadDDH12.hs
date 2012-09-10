{-# LANGUAGE RecordWildCards #-}

module Main where


import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import           LoadUtils


getDefinitions :: Document -> [(T.Text, T.Text)]
getDefinitions doc = zip names $ filter (/= "\8212") texts
    where
        cursor = fromDocument doc
        quotes = descendant cursor >>= element "h3" >>= following >>= element "p"
        texts  = quotes >>= child >>= content
        names  = quotes >>= child >>= element "em" >>= child >>= content

main :: IO ()
main = loadMain "Day of DH 2012"
                "usage: loadDDH12 USERNAME DB_CXN_STR INPUT_FILE SRC"
                getDefinitions

