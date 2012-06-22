{-# LANGUAGE ScopedTypeVariables #-}

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

import           Data.Maybe
import           Data.Monoid
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
            let (content, hash) = getContent docInfo
                doc = Document (diTitle docInfo)
                               (getSource docInfo)
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
    admin         <- (== Authorized) <$> isAdmin
    defaultLayout $ do
        setTitle . (mappend "What is DH? ") . toHtml $ documentTitle doc
        $(widgetFile "doc")

postDocR :: DocumentId -> Handler RepHtml
postDocR docId = do
    mdoc <- Just <$> (runDB $ get404 docId)
    ((result, form), enctype) <- runFormPost $ docForm mdoc
    case result of
        FormSuccess updated -> do
            let (content, hash) = getContent updated
            runDB $ update docId [ DocumentTitle   =. diTitle updated
                                 , DocumentSource  =. getSource updated
                                 , DocumentHash    =. hash
                                 , DocumentContent =. content
                                 ]
            redirect $ DocR docId
        _ -> do
            let action = DocR docId
            defaultLayout $(widgetFile "docedit")

-- DocEditR

getDocEditR :: DocumentId -> Handler RepHtml
getDocEditR docId = do
    mdoc <- Just <$> (runDB $ get404 docId)
    let action = DocR docId
    ((result, form), enctype) <- runFormPost $ docForm mdoc
    defaultLayout $(widgetFile "docedit")

-- DocDeleteR

getDocDeleteR :: DocumentId -> Handler RepHtml
getDocDeleteR docId = do
    doc <- runDB $ get404 docId
    defaultLayout $(widgetFile "docdelete")

postDocDeleteR :: DocumentId -> Handler RepHtml
postDocDeleteR docId = do
    sure :: Int <- runInputPost $ ireq intField "sure"
    case sure of
        1 -> do
            runDB $ delete docId
            setMessage "Document deleted."
            redirect DocListR
        _ -> redirect $ DocR docId

-- Forms

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

