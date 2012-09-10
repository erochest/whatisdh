{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module LoadUtils
    ( loadDocument
    , loadMain
    ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Postgresql
import           Filesystem.Path.CurrentOS
import           Control.Monad.IO.Class
import qualified Model as Mdl
import           System.Environment
import qualified Text.HTML.DOM as H
import           Text.Printf
import qualified Text.XML as X

loadDocument :: (PersistStore b m, MonadIO (b m))
             => T.Text
             -> (X.Document -> [(T.Text, T.Text)])
             -> Key t (Mdl.UserGeneric t)
             -> X.Document
             -> T.Text
             -> b m ()
loadDocument title getDefs userId doc src = do
    now <- liftIO getCurrentTime
    let docs = M.elems . M.fromList . map (docpair . todoc now) $ getDefs doc
    forM_ docs $ \d -> do
        _ <- liftIO . printf ">>> %s\n" $ show d
        _ <- insert d
        return ()
    where
        todoc time (name, def) =
            Mdl.Document (title <> " -- " <> name)
                         (Just src)
                         userId
                         time
                         (Mdl.makeHash def)
                         def
                         Nothing
        docpair d@Mdl.Document{..} = (documentHash, d)

loadMain :: T.Text
         -> T.Text
         -> (X.Document -> [(T.Text, T.Text)])
         -> IO ()
loadMain title usage getDefs = do
    args <- getArgs
    case args of
        [user, dbCxnStr, inputFile, src] -> do
            input <- H.readFile . fromText $ T.pack inputFile
            _ <- printf "Attempting connection %s\n" dbCxnStr
            withPostgresqlConn (C8.pack dbCxnStr) $ runSqlConn $ do
                liftIO $ putStrLn "Connection made!"
                muser <- getBy . Mdl.UniqueUser $ T.pack user
                _ <- liftIO $ printf "Retrieved user: %s\n" (show muser)
                case muser of
                    Just (Entity userId _) ->
                        loadDocument title getDefs userId input $ T.pack src
                    Nothing -> return ()
        _ -> putStrLn (T.unpack usage)

