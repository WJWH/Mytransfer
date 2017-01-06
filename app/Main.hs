{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import ImageProvider
import StorageBackend
import Mailer

main :: IO ()
main = do
    getBackgroundPath <- startBackgroundProvider --returns a function that returns a image filepath
    scotty 3000 $ do
        middleware logStdoutDev
        get "/" showLandingPage
        get "/background" $ serveBackground getBackgroundPath
        post "/upload" uploadFile
        get "/download" downloadFile --should arguably be a POST, as it can't be cached

showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html" --file doesn't set the content type by itself
    file "homepage.html" --body of the response is a file (in this case the homepage)

serveBackground :: (IO FilePath) -> ActionM ()
serveBackground getBackgroundPath = do
    filepath <- liftIO getBackgroundPath --see the ImageProvider module for how this works
    setHeader "Content-Type" "image/jpeg" -- all the images are JPEGs
    file filepath --serve the background image

-- /upload
uploadFile :: ActionM ()
uploadFile = do
    mailadress <- param "email" :: ActionM T.Text
    fs <- (map snd) <$> files --fst bit of the tuple is not needed
    filenames <- liftIO $ mapM addFile fs
    --send a mail with the filenames
    liftIO $ sendUploadedFilesMessage filenames mailadress
    status status204

-- /download
downloadFile :: ActionM ()
downloadFile = do
    fileID <- param "fid" :: ActionM T.Text
    mfp <- liftIO $ retrieveFile fileID
    case mfp of
        Nothing -> do --either the file was downloaded too many times or it doesn't exist
            status status404
            html "The file could not be found. Maybe it has expired or has been downloaded too many times already?"
        Just fp -> do
            setHeader "Content-Type" "application/octet-stream"
            setHeader "Content-Disposition" ("attachment; filename=" <> (TL.fromStrict fileID))
            file fp
