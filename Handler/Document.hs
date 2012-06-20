
module Handler.Document
    ( getDocListR
    , getDocNewR
    , postDocNewR
    , getDocR
    , postDocR
    , getDocEditR
    , getDocDeleteR
    , postDocDeleteR
    , docAForm
    , docForm
    ) where

import           Data.Monoid
import           Import
import           Text.Blaze (Markup)
import           Yesod.Auth

-- DocListR

getDocListR :: Handler RepHtml
getDocListR = undefined

-- DocNewR

getDocNewR :: Handler RepHtml
getDocNewR = undefined

postDocNewR :: Handler RepHtml
postDocNewR = undefined

-- DocR

getDocR :: DocumentId -> Handler RepHtml
getDocR docId = undefined

postDocR :: DocumentId -> Handler RepHtml
postDocR docId = undefined

-- DocEditR

getDocEditR :: DocumentId -> Handler RepHtml
getDocEditR docId = undefined

-- DocDeleteR

getDocDeleteR :: DocumentId -> Handler RepHtml
getDocDeleteR docId = undefined

postDocDeleteR :: DocumentId -> Handler RepHtml
postDocDeleteR docId = undefined

-- Forms

docAForm :: (Yesod m, RenderMessage m FormMessage)
         => Maybe Document -> AForm s m Document
docAForm mdoc = undefined

docForm :: (Yesod m, RenderMessage m FormMessage)
        => Maybe Document
        -> Markup
        -> MForm s m (FormResult Document, GWidget s m ())
docForm = renderBootstrap . docAForm

