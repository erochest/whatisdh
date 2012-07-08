{-# LANGUAGE RankNTypes #-}

module Utils.Auth
    ( authRole
    , selfOrRole
    , getKeyedUser
    , keyAuthRole
    , isSuper
    , isAdmin
    ) where

import           Control.Applicative ((<$>))
import qualified Data.Text as T
import           Model
import           Prelude
import           Yesod.Auth
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist

-- Authentication utilities

unauthMessage :: T.Text
unauthMessage = "You are not authorized to view this."

authRole :: forall m val s.
            ( YesodPersist m
            , PersistStore (PersistEntityBackend val) (GHandler s m)
            , PersistEntity val
            , YesodAuth m
            , AuthId m ~ Key (PersistEntityBackend val) val
            , YesodPersistBackend m ~ PersistEntityBackend val
            )
         => (val -> Bool)
         -> GHandler s m AuthResult
authRole predicate = do
    muser <- maybeAuth
    return $ case muser of
        Nothing -> AuthenticationRequired
        Just (Entity _ user) | predicate user -> Authorized
                             | otherwise -> Unauthorized unauthMessage

selfOrRole :: forall m val s.
              ( YesodPersist m
              , PersistStore (PersistEntityBackend val) (GHandler s m)
              , PersistEntity val
              , YesodAuth m
              , AuthId m ~ Key (PersistEntityBackend val) val
              , YesodPersistBackend m ~ PersistEntityBackend val
              )
           => (val -> Bool)
           -> Key (PersistEntityBackend val) val
           -> GHandler s m AuthResult
selfOrRole predicate selfId = do
    muser <- maybeAuth
    return $ case muser of
        Nothing -> AuthenticationRequired
        Just (Entity uid user) | uid == selfId -> Authorized
                               | predicate user -> Authorized
                               | otherwise -> Unauthorized unauthMessage

-- API key authentication

newtype ApiKey = ApiKey (Maybe T.Text)

apiKeyForm :: (RenderMessage m FormMessage) => FormInput s m ApiKey
apiKeyForm = ApiKey <$> iopt textField "key"

getKeyedUser :: forall sub master.
                ( RenderMessage master FormMessage
                , YesodPersist master
                , PersistUnique (YesodPersistBackend master) (GHandler sub master)
                )
             => GHandler sub master (Maybe (Entity (UserGeneric (YesodPersistBackend master))))
getKeyedUser = do
    ApiKey mkey <- runInputGet apiKeyForm
    case mkey of
        Nothing  -> return Nothing
        Just key -> runDB . getBy $ UniqueApiKey key

keyAuthRole :: forall sub master.
               ( YesodPersist master
               , PersistUnique (YesodPersistBackend master) (GHandler sub master)
               , YesodAuth master
               , AuthId master ~ Key (YesodPersistBackend master)
                                     (UserGeneric (YesodPersistBackend master))
               )
            => (UserGeneric (YesodPersistBackend master) -> Bool)
            -> GHandler sub master AuthResult
keyAuthRole predicate = do
    mkuser <- getKeyedUser
    case mkuser of
        Nothing -> authRole predicate
        Just (Entity _ user) | predicate user -> return Authorized
                             | otherwise -> return $ Unauthorized unauthMessage

-- User-related

isSuper :: forall m s.
           ( YesodPersist m
           , PersistStore (YesodPersistBackend m) (GHandler s m)
           , YesodAuth m
           , AuthId m ~ Key (YesodPersistBackend m)
                            (UserGeneric (YesodPersistBackend m))
           )
        => GHandler s m AuthResult
isSuper = authRole userSuper

isAdmin :: forall m s.
           ( YesodPersist m
           , PersistStore (YesodPersistBackend m) (GHandler s m)
           , YesodAuth m
           , AuthId m ~ Key (YesodPersistBackend m)
                            (UserGeneric (YesodPersistBackend m))
           )
        => GHandler s m AuthResult
isAdmin = authRole userAdmin

