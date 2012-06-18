
module Handler.User
    ( getUserListR
    , getUserR
    , postUserR
    , postUserDelR
    ) where

import           Data.Monoid
import           Import

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

postUserDelR :: UserId -> Handler RepHtml
postUserDelR uid = undefined

