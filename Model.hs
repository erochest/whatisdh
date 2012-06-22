{-# LANGUAGE RankNTypes #-}
module Model where

import qualified Data.ByteString.Lazy as BSL
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Prelude
import           Yesod
import           Yesod.Auth
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time
import           Database.Persist.Quasi

data TokenCategory
    = AlphaToken
    | NumberToken
    | PunctuationToken
    | SymbolToken
    | MarkToken
    deriving (Show, Read, Eq)
derivePersistField "TokenCategory"


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

isSuper :: forall m s
         . ( YesodAuth m
           , PersistStore (YesodPersistBackend m) (GHandler s m)
           , YesodPersist m
           , AuthId m ~ Key (YesodPersistBackend m) (UserGeneric (YesodPersistBackend m))
           )
        => GHandler s m AuthResult
isSuper = do
    muser <- maybeAuth
    return $ case muser of
        Nothing                         -> AuthenticationRequired
        Just (Entity _ (User _ True _)) -> Authorized
        Just _                          -> Unauthorized "You have to be super."

isAdmin :: forall m s
         . ( YesodAuth m
           , PersistStore (YesodPersistBackend m) (GHandler s m)
           , YesodPersist m
           , AuthId m ~ Key (YesodPersistBackend m) (UserGeneric (YesodPersistBackend m))
           )
        => GHandler s m AuthResult
isAdmin = do
    muser <- maybeAuth
    return $ case muser of
        Nothing -> AuthenticationRequired
        Just (Entity _ (User _ _ True)) -> Authorized
        Just _ -> Unauthorized "You have to be an admin."

makeHash :: T.Text -> T.Text
makeHash = T.pack . showDigest . sha1 . BSL.fromChunks . (:[]) . encodeUtf8

