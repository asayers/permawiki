{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Default.Class (def)
import Data.Hashable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Markdown as MD
import System.Directory
import System.FilePath ((</>))
import Web.PathPieces
import Web.Spock.Safe
import Web.Spock.Shared

markdown :: T.Text -> ActionT IO a
markdown = html . TL.toStrict . renderHtml . MD.markdown def . TL.fromStrict

main :: IO ()
main = runSpock 9876 $ spockT id $ do
    get root $
        text "Hello World!"
    get "new" $ newPageView
    post "new" $ do
        contents <- param' "contents"
        (pageId, pageHash) <- liftIO $ createPage contents
        redirect $ "/perma/" <> showHash pageHash
    get ("edit" <//> var) $ editPageView
    post ("edit" <//> var) $ \pageId -> do
        contents <- param' "contents"
        (pageId, pageHash) <- liftIO $ editPage pageId contents
        redirect $ "/perma/" <> showHash pageHash
    get ("perma" <//> var) $ \pageHash -> do
        unless (verifyHash pageHash) $ error "invalid hash"
        let filepath = "perma" </> pageHash
        exists <- liftIO $ doesFileExist filepath
        unless exists $ error "file not found"
        contents <- liftIO $ T.readFile filepath
        unless (verifyContents pageHash contents) $ error "invalid contents"
        markdown contents

newPageView :: ActionT IO a
newPageView = do
    html $ T.unlines
        [ "<h1>New page</h1>"
        , "<form action='/new' method='POST'>"
        , "Contents:<br>"
        , "<input type='text' name='contents'>"
        , "<br><br>"
        , "<input type='submit' value='Submit'>"
        , "</form>"
        ]

editPageView :: PageId -> ActionT IO a
editPageView pageId = do
    html $ T.unlines
        [ "<h1>Page Id: " <> toPathPiece pageId <> "</h1>"
        , "<form action='/edit/" <> toPathPiece pageId <> "' method='POST'>"
        , "Contents:<br>"
        , "<input type='text' name='contents'>"
        , "<br><br>"
        , "<input type='submit' value='Submit'>"
        , "</form>"
        ]

verifyHash _ = True
verifyContents _ _ = True

newtype PageId = PageId { unPageId :: UUID.UUID } deriving Show
newtype PageHash = PageHash { unPageHash :: Int } deriving Show

instance PathPiece PageId where
    fromPathPiece = fmap PageId . UUID.fromString . T.unpack
    toPathPiece = T.pack . UUID.toString . unPageId

newPageId :: IO PageId
newPageId = PageId <$> UUID.nextRandom

showHash :: PageHash -> T.Text
showHash = T.pack . show . unPageHash

mkHash :: T.Text -> PageHash
mkHash = PageHash . hash

createPage :: T.Text -> IO (PageId, PageHash)
createPage contents = do
    pageId <- newPageId
    pageHash <- writeNewPageInstanceToDisk contents
    appendVersionToLog pageId pageHash
    return (pageId, pageHash)

editPage :: PageId -> T.Text -> IO (PageId, PageHash)
editPage pageId contents = do
    pageHash <- writeNewPageInstanceToDisk contents
    appendVersionToLog pageId pageHash
    return (pageId, pageHash)

writeNewPageInstanceToDisk :: T.Text -> IO PageHash
writeNewPageInstanceToDisk contents = do
    let pageHash = mkHash contents
    let filepath = "perma" </> T.unpack (showHash pageHash)
    exists <- liftIO $ doesFileExist filepath
    when exists $ error "file already exists"
    T.writeFile filepath contents
    return pageHash

appendVersionToLog :: PageId -> PageHash -> IO ()
appendVersionToLog pageId pageHash = do
    let filepath = "pages" </> T.unpack (toPathPiece pageId)
    T.appendFile filepath $ showHash pageHash <> "\n"
