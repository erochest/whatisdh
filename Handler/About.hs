
module Handler.About where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "What is DH? | About"
    $(widgetFile "about")

