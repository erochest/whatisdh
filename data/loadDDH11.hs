{-# LANGUAGE RecordWildCards #-}

module Main where


import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import           Text.XML.Cursor as C
import           LoadUtils (loadMain)


getDefinitions :: Document -> [(T.Text, T.Text)]
getDefinitions doc = zip names texts
    where
        cursor  = fromDocument doc
        headers = descendant cursor >>=
                  element "h2" >>= child >>=
                  element "span" >>= attributeIs "class" "mw-headline" >>=
                  C.parent
        quotes  = headers >>= following >>= element "p" >>= child
        texts   = quotes >>= content
        names   = quotes >>= element "i" >>= child >>= content

main :: IO ()
main = loadMain "Day of DH 2009-2011"
                "usage: loadDDH12 USERNAME DB_CXN_STR INPUT_FILE SRC"
                getDefinitions

