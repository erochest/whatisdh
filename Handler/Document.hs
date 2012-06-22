
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time
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
postDocNewR = do
    let action = DocNewR
        mdoc   = Nothing
    ((result, form), enctype) <- runFormPost $ docForm Nothing
    case result of
        FormSuccess docInfo -> do
            userId <- requireAuthId
            now    <- liftIO getCurrentTime
            let content = maybe "" id . listToMaybe $ catMaybes
                    [ unTextarea <$> diContent docInfo
                    , (decodeUtf8 . toStrict . fileContent) <$> diFile docInfo
                    ]
                hash = T.pack . showDigest . sha1 $ BSL.fromChunks [encodeUtf8 content]
                doc = Document (diTitle docInfo)
                               (diSource docInfo)
                               userId
                               now
                               hash
                               content
            docId <- runDB $ insert doc
            redirect $ DocR docId
        FormFailure msgs -> do
            setMessage $ listToUl msgs
            defaultLayout $(widgetFile "docedit")
        FormMissing -> defaultLayout $(widgetFile "docedit")

-- DocR

getDocR :: DocumentId -> Handler RepHtml
getDocR docId = do
    currentUserId <- maybeAuthId
    doc           <- runDB $ get404 docId
    uploadingUser <- runDB . get $ documentUploadedBy doc
    defaultLayout $ do
        setTitle . (mappend "What is DH? ") . toHtml $ documentTitle doc
        $(widgetFile "doc")

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
    , diContent :: Maybe Textarea
    , diFile    :: Maybe FileInfo
    }

docAForm :: (Yesod m, RenderMessage m FormMessage)
         => Maybe Document -> AForm s m DocumentInfo
docAForm mdoc =   DocumentInfo
              <$> areq textField     "Title"       (documentTitle  <$> mdoc)
              <*> aopt textField     "Source"      (documentSource <$> mdoc)
              <*> aopt textareaField "Content"     (content mdoc)
              <*> fileAFormOpt       "Upload File"
    where
        content :: Maybe Document -> Maybe (Maybe Textarea)
        content = maybe Nothing (Just . Just . Textarea . documentContent)

docForm :: (Yesod m, RenderMessage m FormMessage)
        => Maybe Document
        -> Markup
        -> MForm s m (FormResult DocumentInfo, GWidget s m ())
docForm = renderBootstrap . docAForm

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks

