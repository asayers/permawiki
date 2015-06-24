{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Default.Class (def)
import Data.Hashable
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import Data.Typeable
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Markdown as MD
import System.Directory
import System.FilePath ((</>), takeBaseName)
import Web.PathPieces
import Web.Spock.Safe
import Web.Spock.Shared

renderMarkdown :: T.Text -> T.Text
renderMarkdown = TL.toStrict . renderHtml . MD.markdown def . TL.fromStrict

main :: IO ()
main = runSpock 9876 $ spockT id $ do
    get root $ do
        pages <- liftIO getPages
        html $ renderMarkdown $ rootView pages
    get "new" $
        html newPageView
    post "new" $ do
        contents <- param' "contents"
        (pageId, pageHash) <- liftIO $ createPage contents
        redirect $ "/page/" <> toPathPiece pageId

    get ("edit" <//> var) $ \pageId -> do
        versions <- liftIO $ getPageVersions pageId
        html $ T.unlines
            [ "<h2>You're editing page <pre>" <> toPathPiece pageId <> "</pre></h2>"
            , renderMarkdown $ pageHeader pageId versions
            , editPageView pageId
            ]
    post ("edit" <//> var) $ \pageId -> do
        contents <- param' "contents"
        (pageId, pageHash) <- liftIO $ editPage pageId contents
        redirect $ "/page/" <> toPathPiece pageId

    get ("page" <//> var) $ \pageId -> do
        versions <- liftIO $ getPageVersions pageId
        contents <- liftIO $ runMaybeT $
            MaybeT (return $ preview (ix 0) (reverse versions)) >>=
            MaybeT . getPageInstanceContents
        html $ renderMarkdown $ T.unlines
            [ "## You're viewing page `" <> toPathPiece pageId <> "`"
            , pageHeader pageId versions
            , fromMaybe "not found" contents
            ]
    get ("perma" <//> var) $ \pageHash -> do
        contents <- liftIO $ getPageInstanceContents pageHash
        maybe notFound (html . renderMarkdown) contents

-- TODO: Status 404
notFound = text "not found"

rootView :: [PageId] -> T.Text
rootView pageIds = T.unlines $
    [ "# Welcome to PermaWiki"
    , ""
    , "Lorem ipsum dolor sit amet..."
    , ""
    , "## Pages"
    , ""
    ] ++ map (ppPage . toPathPiece) pageIds ++
    [ "- [Create new page](/new/)"
    ]
  where
    ppPage p = "- [" <> p <> "](/page/" <> p <> ")"

pageHeader :: PageId -> [PageHash] -> T.Text
pageHeader pageId versions = T.unlines $
    [ "You're viewing page `" <> toPathPiece pageId <> "`. Versions:"
    , ""
    ] ++ map (ppVersion . showHash) versions ++
    [ "- [Create new version](/edit/" <> toPathPiece pageId <> ")"
    , "<hr>"
    ]
  where
    ppVersion v = "- [" <> v <> "](/perma/" <> v <> ")"

newPageView :: T.Text
newPageView = T.unlines
    [ "<h1>New page</h1>"
    , "<form action='/new' method='POST'>"
    , "Contents:<br>"
    , "<input type='text' name='contents'>"
    , "<br><br>"
    , "<input type='submit' value='Submit'>"
    , "</form>"
    ]

editPageView :: PageId -> T.Text
editPageView pageId = T.unlines
    [ "<h1>Page Id: " <> toPathPiece pageId <> "</h1>"
    , "<form action='/edit/" <> toPathPiece pageId <> "' method='POST'>"
    , "Contents:<br>"
    , "<input type='text' name='contents'>"
    , "<br><br>"
    , "<input type='submit' value='Submit'>"
    , "</form>"
    ]

newtype PageId = PageId { unPageId :: UUID.UUID } deriving (Show, Typeable)
newtype PageHash = PageHash { unPageHash :: Int } deriving (Show, PathPiece)

instance PathPiece PageId where
    fromPathPiece = fmap PageId . UUID.fromString . T.unpack
    toPathPiece = T.pack . UUID.toString . unPageId

newPageId :: IO PageId
newPageId = PageId <$> UUID.nextRandom

showHash :: PageHash -> T.Text
showHash = T.pack . show . unPageHash

readHash :: T.Text -> Maybe PageHash
readHash txt =
    PageHash <$> either (const Nothing) (Just . fst) (T.decimal txt)

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

pageFilepath :: PageId -> FilePath
pageFilepath pageId = "pages" </> T.unpack (toPathPiece pageId)

pageInstanceFilepath :: PageHash -> FilePath
pageInstanceFilepath pageHash = "perma" </> T.unpack (showHash pageHash)

appendVersionToLog :: PageId -> PageHash -> IO ()
appendVersionToLog pageId pageHash = do
    T.appendFile (pageFilepath pageId) $ showHash pageHash <> "\n"

getLatestPageVersion :: PageId -> IO (Maybe PageHash)
getLatestPageVersion pageId = do
    versions <- getPageVersions pageId
    return $ case reverse versions of
        []    -> Nothing
        (x:_) -> Just x

getPageVersions :: PageId -> IO [PageHash]
getPageVersions pageId = do
    contents <- safeReadFile $ pageFilepath pageId
    let versionsTxt = maybe [] T.lines contents
    return $ mapMaybe readHash versionsTxt

getPageInstanceContents :: PageHash -> IO (Maybe T.Text)
getPageInstanceContents pageHash = do
    safeReadFile $ pageInstanceFilepath pageHash

getPages :: IO [PageId]
getPages = do
    filenames <- getDirectoryContents "pages"
    return $ mapMaybe (fromPathPiece . T.pack . takeBaseName) filenames

safeReadFile :: FilePath -> IO (Maybe T.Text)
safeReadFile filepath = do
    exists <- doesFileExist filepath
    if exists
        then Just <$> T.readFile filepath
        else return Nothing
