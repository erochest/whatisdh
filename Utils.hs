{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( listToUl
    , toStrict
    , visible
    , Pagination(..)
    , paginationForm
    ) where


import           Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text as T
import           Text.Blaze.Html5
import           Prelude
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist


listToUl :: [T.Text] -> Html
listToUl = ul . L.foldr toli (toHtml T.empty)
    where toli :: T.Text -> Html -> Html
          toli item rest = li (toHtml item) `mappend` rest

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks

visible :: Yesod a => Route a -> Bool -> GHandler s a Bool
visible route write = visible' <$> isAuthorized route write
    where
        visible' Authorized = True
        visible' _          = False

data Pagination = Pagination
    { paginationOffset  :: Maybe Int
    , paginationLimit   :: Maybe Int
    , paginationOrderBy :: Maybe T.Text
    , paginationSort    :: Maybe T.Text
    }

paginationForm :: (RenderMessage m FormMessage) => FormInput s m Pagination
paginationForm =   Pagination
               <$> iopt intField  "offset"
               <*> iopt intField  "limit"
               <*> iopt textField "orderby"
               <*> iopt textField "sort"

