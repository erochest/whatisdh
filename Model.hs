{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
module Model where

import           Control.Applicative ((<$>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time
import           Database.Persist.Quasi
import           Utils
import           Prelude
import           Yesod

data TokenCategory
    = AlphaToken
    | NumberToken
    | PunctuationToken
    | SymbolToken
    | MarkToken
    | WhiteSpaceToken
    | UnknownToken
    | CompositeToken
    deriving (Show, Read, Eq)
derivePersistField "TokenCategory"


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- Document-related

data DocumentInfo = DocumentInfo
    { diTitle   :: T.Text
    , diSource  :: Maybe T.Text
    , diContent :: Maybe Textarea
    , diFile    :: Maybe FileInfo
    }

makeHash :: T.Text -> T.Text
makeHash = T.pack . showDigest . sha1 . BSL.fromChunks . (:[]) . encodeUtf8

getContent :: DocumentInfo -> (T.Text, T.Text)
getContent dinfo = (content, hash)
    where content = maybe "" id . listToMaybe $ catMaybes
                [ unTextarea <$> diContent dinfo
                , (decodeUtf8 . toStrict . fileContent) <$> diFile dinfo
                ]
          hash = makeHash content

getSource :: DocumentInfo -> Maybe T.Text
getSource dinfo = listToMaybe $ catMaybes [ diSource dinfo
                                          , fileName <$> diFile dinfo
                                          ]

instance A.ToJSON a => A.ToJSON (Entity a) where
    toJSON (Entity i e) = A.object [ "id"     .= i
                                   , "entity" .= e
                                   ]

instance A.ToJSON User where
    toJSON (User i s a k) = A.object [ "ident"  .= i
                                     , "super"  .= s
                                     , "admin"  .= a
                                     , "apikey" .= k
                                     ]


