
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

import           Data.Time
import           Data.Monoid
import qualified Data.Text as T
import           Import
import           Text.Blaze (Markup)
import           Yesod.Auth

-- DocListR

getDocListR :: Handler RepHtml
getDocListR = do
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        setTitle "What is DH? Documents"
        $(widgetFile "doclist")

-- DocNewR

getDocNewR :: Handler RepHtml
getDocNewR = do
    let action = DocNewR
        mdoc   = Nothing
    ((result, form), enctype) <- runFormPost $ docForm Nothing
    defaultLayout $(widgetFile "docedit")

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

data DocumentInfo = DocumentInfo
    { diTitle   :: T.Text
    , diSource  :: Maybe T.Text
    , diContent :: Maybe T.Text
    , diFile    :: Maybe FileInfo
    }

docAForm :: (Yesod m, RenderMessage m FormMessage)
         => Maybe Document -> AForm s m DocumentInfo
docAForm mdoc =   DocumentInfo
              <$> areq textField "Title"   (documentTitle   <$> mdoc)
              <*> aopt textField "Source"  (documentSource  <$> mdoc)
              <*> aopt textField "Content" (Just . documentContent <$> mdoc)
              <*> fileAFormOpt "Upload File"

docForm :: (Yesod m, RenderMessage m FormMessage)
        => Maybe Document
        -> Markup
        -> MForm s m (FormResult DocumentInfo, GWidget s m ())
docForm = renderBootstrap . docAForm

