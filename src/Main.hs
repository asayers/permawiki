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
        pageId <- param' "page-id"
        contents <- param' "contents"
        pageHash <- liftIO $ createPage pageId contents
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
        [ "<form action='/new' method='POST'>"
        , "Page Id:<br>"
        , "<input type='text' name='page-id'>"
        , "<br>"
        , "Contents:<br>"
        , "<input type='text' name='contents'>"
        , "<br><br>"
        , "<input type='submit' value='Submit'>"
        , "</form>"
        ]

verifyHash _ = True
verifyContents _ _ = True

newtype PageId = PageId Int deriving (Show, PathPiece)
newtype PageHash = PageHash { unPageHash :: Int } deriving Show

showHash :: PageHash -> T.Text
showHash = T.pack . show . unPageHash

mkHash :: T.Text -> PageHash
mkHash = PageHash . hash

createPage :: PageId -> T.Text -> IO PageHash
createPage pageId contents = do
    let pageHash = mkHash contents
    let filepath = "perma" </> T.unpack (showHash pageHash)
    exists <- liftIO $ doesFileExist filepath
    when exists $ error "file already exists"
    T.writeFile filepath contents
    return $ mkHash contents
