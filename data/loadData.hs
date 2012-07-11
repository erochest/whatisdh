{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative hiding (many)
import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Resource
import           Data.Conduit (($$))
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import           Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Database.Persist.Postgresql
import           Database.Persist.Store
import           Model
import           Shelly
import           System.Environment
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Parse as XP
import           Yesod.Default.Config (DefaultEnv(..), withYamlEnvironment)
default (LT.Text)

data Post = Post
    { id                    :: Int
    , parentId              :: Maybe Int
    , postTypeId            :: Int
    , acceptedAnswerId      :: Maybe Int
    , creationDate          :: T.Text  -- UTCTime
    , score                 :: Int
    , viewCount             :: Int
    , postBody              :: T.Text
    , ownerUserId           :: Maybe Int
    , lastEditorUserId      :: Maybe Int
    , lastEditorDisplayName :: Maybe T.Text
    , lastEditDate          :: Maybe T.Text  -- UTCTime
    , lastActivityDate      :: T.Text        -- UTCTime
    , communityOwnedDate    :: Maybe T.Text
    , closedDate            :: Maybe T.Text
    , postTitle             :: Maybe T.Text
    , postTags              :: Maybe T.Text
    , answerCount           :: Maybe Int
    , commentCount          :: Maybe Int
    , favoriteCount         :: Maybe Int
    }
    deriving (Show)

parsePosts = tagNoAttr "posts" $ many parseRow

parseRow = tagName "row" attrs return
    where
        attrs :: AttrParser Post
        attrs = Post <$> (read'  <$> requireAttr "Id")
                     <*> (fread' <$> optionalAttr "ParentId")
                     <*> (read'  <$> requireAttr "PostTypeId")
                     <*> (fread' <$> optionalAttr "AcceptedAnswerId")
                     <*> (requireAttr "CreationDate")
                     <*> (read'  <$> requireAttr "Score")
                     <*> (read'  <$> requireAttr "ViewCount")
                     <*> (requireAttr "Body")
                     <*> (fread' <$> optionalAttr "OwnerUserId")
                     <*> (fread' <$> optionalAttr "LastEditorUserId")
                     <*> (optionalAttr "LastEditorDisplayName")
                     <*> (optionalAttr "LastEditDate")
                     <*> (requireAttr "LastActivityDate")
                     <*> (optionalAttr "CommunityOwnedDate")
                     <*> (optionalAttr "ClosedDate")
                     <*> (optionalAttr "Title")
                     <*> (optionalAttr "Tags")
                     <*> (fread' <$> optionalAttr "AnswerCount")
                     <*> (fread' <$> optionalAttr "CommentCount")
                     <*> (fread' <$> optionalAttr "FavoriteCount")

        read'  = read . T.unpack

        fread' = fmap read'

postToDocument :: Shelly.FilePath -> UserId -> Post -> IO Document
postToDocument source userId post = do
    let body = postBody post
    now <- getCurrentTime
    return $ Document (maybe "<untitled>" Prelude.id $ postTitle post)
                      (Just . LT.toStrict $ toTextIgnore source)
                      userId
                      now
                      (makeHash body)
                      body

getUserId dbconf username = 
    withPostgresqlConn (pgConnStr dbconf) $ runSqlConn $ do
        muser <- getBy (UniqueUser username)
        return $ case muser of
            Just (Entity userId _) -> Just userId
            Nothing -> Nothing

readPosts inputFile =
    runResourceT $  XP.parseFile def inputFile
                 $$ force "rows required" parsePosts

loadData dbconf userId inputFiles = forM_ inputFiles $ \inputFile -> do
    start <- liftIO getCurrentTime
    echo (toTextIgnore inputFile)
    rows <- liftIO $ readPosts inputFile
    docs <- liftIO $ mapM (postToDocument inputFile userId) rows
    let docSet = M.elems $ L.foldl' (\m d -> M.insert (documentHash d) d m)
                                    M.empty docs
    inspect ("row count:", length rows)
    inspect ("unqiue count:", length docSet)
    liftIO $ withPostgresqlConn (pgConnStr dbconf) $ runSqlConn $ do
        forM_ docSet $ \d -> do
            -- liftIO . putStrLn . (">>> " ++) $ show d
            _ <- insert d
            return ()
    end <- liftIO getCurrentTime
    inspect ("done", end `diffUTCTime` start)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (configFile:userName:inputFiles) -> do
            dbconf  <- withYamlEnvironment configFile Development loadConfig
            mUserId <- getUserId dbconf $ T.pack userName
            case mUserId of
                Just userId -> shelly $ verbosely $
                    loadData dbconf userId $ map (fromText . LT.pack) inputFiles
                Nothing -> putStrLn "Invalid user name."

        _ -> putStrLn "usage: loadData DB_CONFIG_FILE USER_NAME INPUT_FILE [INPUT_FILE...]"

