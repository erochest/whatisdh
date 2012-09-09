
module Handler.About where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setSiteTitleMsg " | About"
    $(widgetFile "about")

