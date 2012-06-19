
module Handler.User
    ( getUserListR
    , getUserR
    , postUserR
    , deleteUserR
    , getUserEditR
    , userAForm
    , userForm
    ) where

import           Data.Monoid
import           Import
import           Text.Blaze (Markup)

getUserListR :: Handler RepHtml
getUserListR = defaultLayout $ do
    users <- lift . runDB $ selectList [] [Asc UserIdent]
    setTitle "What is DH? Users"
    $(widgetFile "userlist")

getUserR :: UserId -> Handler RepHtml
getUserR uid = defaultLayout $ do
    user <- lift . runDB $ get404 uid
    setTitle . ("What is DH? " `mappend`) . toHtml $ userIdent user
    $(widgetFile "user")

postUserR :: UserId -> Handler RepHtml
postUserR uid = undefined

deleteUserR :: UserId -> Handler RepHtml
deleteUserR uid = undefined

getUserEditR :: UserId -> Handler RepHtml
getUserEditR uid = defaultLayout $ do
    user <- lift . runDB $ get404 uid
    ((result, form), enctype) <- lift . runFormPost . userForm $ Just user
    $(widgetFile "useredit")

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

