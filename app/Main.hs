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
        get "/uploadframe" showUploadFrame
        get "/background" $ serveBackground getBackgroundPath
        post "/upload" uploadFile
        get "/download" downloadFile --should arguably be a POST, as it can't be cached

showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html" --file doesn't set the content type by itself
    file "homepage.html" --body of the response is a file (in this case the homepage)

showUploadFrame :: ActionM ()
showUploadFrame = do
    setHeader "Content-Type" "text/html" --file doesn't set the content type by itself
    file "uploadframe.html" --body of the response is a file (in this case the homepage)

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
    case fs of
        [] -> do
            status status400
            text "No files were submitted."
        fs' -> do
            filenames <- liftIO $ mapM addFile fs'
            --send a mail with the filenames
            mailResult <- liftIO $ sendUploadedFilesMessage filenames mailadress
            case mailResult of
                Left _ -> do
                    status status400 --sending the mail failed
                    text "Mailing the link(s) failed"
                Right _ -> do
                    status status200 --sending the mail succeeded
                    text "You will receive a mail with links to download the files."

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
