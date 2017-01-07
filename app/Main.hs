{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import Web.Scotty

import ImageProvider
import StorageBackend
import Mailer

main :: IO ()
main = do
    getBackgroundPath <- startBackgroundProvider --returns a function that returns a image filepath
    scotty 80 $ do
        middleware logStdout
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
    setHeader "Cache-Control" "no-cache" --otherwise refreshing the page won't give you a new background 
    file filepath --serve the background image

-- /upload
uploadFile :: ActionM ()
uploadFile = do
    mailadress <- param "email" :: ActionM T.Text
    fs <- (map snd) <$> files --fst bit of the tuple is not needed
    case fs of
        [FileInfo "\"\"" _ _ ] -> do --if you select no files in the page, there will still be an empty file submitted
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
        NotFound -> do --either the file was downloaded too many times or it doesn't exist
            status status404
            html "The file you requested could not be found."
        Expired -> do
            status status404
            html "The file you requested was uploaded more than a week ago and has been deleted."
        TooManyDownloads -> do
            status status404
            html "The file you requested has been downloaded too many times already and has been deleted."
        Found fp -> do
            setHeader "Content-Type" "application/octet-stream"
            setHeader "Content-Disposition" ("attachment; filename=" <> (TL.fromStrict fileID))
            file fp
        ServerError -> do
            status status500
            html "Something went wrong while looking up the file you requested."
