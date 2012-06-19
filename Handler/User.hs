{-# LANGUAGE ScopedTypeVariables #-}

module Handler.User
    ( getUserListR
    , getUserR
    , postUserR
    , getUserEditR
    , getUserDeleteR
    , postUserDeleteR
    , userAForm
    , userForm
    ) where

import           Data.Monoid
import           Import
import           Text.Blaze (Markup)
import           Yesod.Auth

isSameUser :: UserId -> Entity User -> Bool
isSameUser uid (Entity eid _) = uid == eid

getUserListR :: Handler RepHtml
getUserListR = do
    isCurrentUser <- isSameUser <$> requireAuthId
    users <- runDB $ selectList [] [Asc UserIdent]
    defaultLayout $ do
        setTitle "What is DH? Users"
        $(widgetFile "userlist")

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
    currentUserId <- requireAuthId
    user <- runDB $ get404 uid
    defaultLayout $ do
        setTitle . ("What is DH? " `mappend`) . toHtml $ userIdent user
        $(widgetFile "user")

postUserR :: UserId -> Handler RepHtml
postUserR uid = do
    user <- runDB $ get404 uid
    ((result, form), enctype) <- runFormPost . userForm $ Just user
    case result of
        FormSuccess updated -> do
            if user /= updated
                then runDB $ update uid [ UserIdent =. userIdent updated
                                        , UserSuper =. userSuper updated
                                        , UserAdmin =. userAdmin updated
                                        ]
                else return ()
            redirect $ UserR uid
        _ -> defaultLayout $(widgetFile "useredit")

getUserEditR :: UserId -> Handler RepHtml
getUserEditR uid = do
    user <- runDB $ get404 uid
    ((result, form), enctype) <- runFormPost . userForm $ Just user
    defaultLayout $(widgetFile "useredit")

userAForm :: (Yesod m, RenderMessage m FormMessage)
          => Maybe User -> AForm s m User
userAForm muser =   User
                <$> areq textField   "Identifier" (userIdent <$> muser)
                <*> areq boolField   sufs         (userSuper <$> muser)
                <*> areq boolField   adfs         (userAdmin <$> muser)
    where
        sufs = FieldSettings "Superuser"
                             Nothing
                             Nothing
                             Nothing
                             [ ("class", "radio inline") ]
        adfs = FieldSettings "Admin"
                             Nothing
                             Nothing
                             Nothing
                             [ ("class", "radio inline") ]

userForm :: (Yesod m, RenderMessage m FormMessage)
         => Maybe User -> Markup -> MForm s m (FormResult User, GWidget s m ())
userForm user = renderBootstrap (userAForm user)

getUserDeleteR :: UserId -> Handler RepHtml
getUserDeleteR uid = do
    user <- runDB $ get404 uid
    defaultLayout $(widgetFile "userdelete")

postUserDeleteR :: UserId -> Handler RepHtml
postUserDeleteR uid = do
    sure :: Int <- runInputPost $ ireq intField "sure"
    case sure of
        1 -> do
            runDB $ delete uid
            redirect UserListR
        _ -> redirect $ UserR uid

