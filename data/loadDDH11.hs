{-# LANGUAGE RecordWildCards #-}

module Main where


import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Postgresql
import           Database.Persist.Store
import           Filesystem.Path.CurrentOS
import qualified Model as M
import           System.Environment
import qualified Text.HTML.DOM as H
import           Text.XML hiding (Document)
import qualified Text.XML as X
import           Text.XML.Cursor
import           Text.XML.Cursor as C
import           Text.Printf


getDefinitions :: X.Document -> [(T.Text, T.Text)]
getDefinitions doc = zip names texts
    where
        cursor  = fromDocument doc
        headers = descendant cursor >>=
                  element "h2" >>= child >>=
                  element "span" >>= attributeIs "class" "mw-headline" >>=
                  C.parent
        quotes  = headers >>= following >>= element "p" >>= child
        texts   = quotes >>= content
        names   = quotes >>= element "i" >>= child >>= content

loadDocument userId doc src = do
    now <- liftIO getCurrentTime
    let docs = M.elems . M.fromList . map (docpair . todoc now) $ getDefinitions doc
    forM_ docs $ \doc -> do
        liftIO . printf ">>> %s\n" $ show doc
        _ <- insert doc
        return ()
    where
        todoc time (name, def) =
            M.Document ("Day of DH 2009-2011 -- " `mappend` name)
                       (Just src)
                       userId
                       time
                       (M.makeHash def)
                       def
                       Nothing
        docpair doc@M.Document{..} = (documentHash, doc)

usage :: IO ()
usage = putStrLn "usage: loadDDH12 USERNAME DB_CXN_STR INPUT_FILE SRC"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [user, dbCxnStr, inputFile, src] -> do
            input <- H.readFile . fromText $ T.pack inputFile
            printf "Attempting connection %s\n" dbCxnStr
            withPostgresqlConn (C8.pack dbCxnStr) $ runSqlConn $ do
                liftIO $ putStrLn "Connection made!"
                muser <- getBy . M.UniqueUser $ T.pack user
                liftIO $ printf "Retrieved user: %s\n" (show muser)
                case muser of
                    Just (Entity userId _) ->
                        loadDocument userId input $ T.pack src
                    Nothing -> return ()

        _ -> usage
    where
        toTextLax fn =
            case toText fn of
                Left  fn' -> fn'
                Right fn' -> fn'

