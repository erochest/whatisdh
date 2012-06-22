{-# LANGUAGE ScopedTypeVariables #-}

module Handler.User
    ( getUserListR
    , getUserR
    , postUserR
    , getUserNewR
    , postUserNewR
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

-- UserListR

getUserListR :: Handler RepHtml
getUserListR = do
    isCurrentUser <- isSameUser <$> requireAuthId
    users <- runDB $ selectList [] [Asc UserIdent]
    defaultLayout $ do
        setTitle "What is DH? Users"
        $(widgetFile "userlist")

-- UserR

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
    currentUserId <- requireAuthId
    user <- runDB $ get404 uid
    defaultLayout $ do
        setTitle . ("What is DH? " `mappend`) . toHtml $ userIdent user
        $(widgetFile "user")

postUserR :: UserId -> Handler RepHtml
postUserR uid = do
    user <- Just <$> (runDB $ get404 uid)
    ((result, form), enctype) <- runFormPost $ userForm user
    case result of
        FormSuccess updated -> do
            if user /= Just updated
                then runDB $ update uid [ UserIdent =. userIdent updated
                                        , UserSuper =. userSuper updated
                                        , UserAdmin =. userAdmin updated
                                        ]
                else return ()
            redirect $ UserR uid
        _ -> do
            let action = UserR uid
            defaultLayout $(widgetFile "useredit")

-- UserNewR

getUserNewR :: Handler RepHtml
getUserNewR = do
    let action = UserNewR
        user   = Nothing
    ((result, form), enctype) <- runFormPost newUserForm
    defaultLayout $(widgetFile "useredit")

postUserNewR :: Handler RepHtml
postUserNewR = do
    ((result, form), enctype) <- runFormPost newUserForm
    case result of
        FormSuccess user ->
            (runDB $ insert user) >> redirect (AuthR LoginR)
        FormFailure msgs -> do
            setMessage $ listToUl msgs
            redirect UserNewR
        FormMissing -> do
            let action = UserNewR
                user   = Nothing
            defaultLayout $(widgetFile "useredit")

-- UserEditR

getUserEditR :: UserId -> Handler RepHtml
getUserEditR uid = do
    let action = UserR uid
    user <- Just <$> (runDB $ get404 uid)
    ((result, form), enctype) <- runFormPost $ userForm user
    defaultLayout $(widgetFile "useredit")

-- UserDeleteR

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
            setMessage "User deleted."
            redirect UserListR
        _ -> redirect $ UserR uid

-- Forms

newUserAForm :: (Yesod m, RenderMessage m FormMessage)
             => AForm s m User
newUserAForm =   (\ident -> User ident False False)
             <$> areq textField "E-Mail" Nothing

newUserForm :: (Yesod m, RenderMessage m FormMessage)
            => Markup -> MForm s m (FormResult User, GWidget s m ())
newUserForm = renderBootstrap newUserAForm

userAForm :: (Yesod m, RenderMessage m FormMessage)
          => Maybe User -> AForm s m User
userAForm muser =   User
                <$> areq textField "E-Mail" (userIdent <$> muser)
                <*> areq boolField sufs     (userSuper <$> muser)
                <*> areq boolField adfs     (userAdmin <$> muser)
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

