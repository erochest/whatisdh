
module Handler.User
    ( getUserListR
    , getUserR
    , postUserR
    , postUserDelR
    ) where

import Import

getUserListR :: Handler RepHtml
getUserListR = defaultLayout $ do
    users <- lift . runDB $ selectList [] [Asc UserIdent]
    setTitle "What is DH? Users"
    $(widgetFile "userlist")

getUserR :: UserId -> Handler RepHtml
getUserR uid = undefined

postUserR :: UserId -> Handler RepHtml
postUserR uid = undefined

postUserDelR :: UserId -> Handler RepHtml
postUserDelR uid = undefined

