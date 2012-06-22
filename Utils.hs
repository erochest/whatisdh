{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( listToUl
    , toStrict
    ) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text as T
import           Text.Blaze.Html5
import           Prelude


listToUl :: [T.Text] -> Html
listToUl = ul . L.foldr toli (toHtml T.empty)
    where toli :: T.Text -> Html -> Html
          toli item rest = li (toHtml item) `mappend` rest

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks

