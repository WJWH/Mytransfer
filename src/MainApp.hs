{-# LANGUAGE OverloadedStrings #-}
module MainApp where


import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Network
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Parse
import Web.Scotty

import ImageProvider
import StorageBackend
import Mailer
import Types
import Utilities
import Autoscaling
import DatabaseCalls

--The endpoints of the app
app getBackgroundPath pool dls shutdownIORef sock loadRef = do
    middleware logStdout
    middleware $ staticPolicy (noDots >-> addBase "static") 
    get "/" showLandingPage
    get "/background" $ serveBackground getBackgroundPath
    post "/upload" $ uploadFileHandler pool
    get "/download" $ downloadFileHandler pool dls --should arguably be a POST, as it has side effects
    post "/dogracefulshutdown" $ shutdownHandler shutdownIORef sock --shut down this server
    get "/healthcheck" $ healthCheckHandler shutdownIORef --for the health check system of the load balancer
    get "/load" $ loadHandler loadRef -- returns the current load on the server in Bytes/sec
    get "/timetodrain" $ drainTimeHandler dls --returns expected time for all current connections to finish
    

--the homepage
showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html" --file doesn't set the content type by itself
    file "static/homepage.html" --body of the response is a file (in this case the homepage)

--serves a different background each time
serveBackground :: (IO FilePath) -> ActionM ()
serveBackground getBackgroundPath = do
    filepath <- liftIO getBackgroundPath --see the ImageProvider module for how this works
    setHeader "Cache-control" "no-cache" --prevent caching the background as that doesn't really make sense in this case anyway
    setHeader "Content-Type" "image/jpeg" -- all the images are JPEGs
    --serve the background image. uses BL.readFile and raw instead of the file response function in order
    --to prevent the server from prematurely returning 304 Not Modified responses
    (liftIO $ BL.readFile filepath) >>= raw

-- /upload
uploadFileHandler :: DBConnectionPool -> ActionM ()
uploadFileHandler pool = do
    mailadress <- param "email" :: ActionM T.Text
    fs <- (map snd) <$> files --fst bit of the tuple is not needed
    case fs of
        [FileInfo "\"\"" _ _ ] -> do --if you select no files in the page, there will still be an empty file submitted
            status status400
            text "No files were submitted."
        fs' -> do
            filenames <- liftIO $ mapM (addFile pool) fs'
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
downloadFileHandler :: DBConnectionPool -> DownloadStore -> ActionM ()
downloadFileHandler pool dls = do
    fileID <- param "fid" :: ActionM T.Text
    mfp <- liftIO $ retrieveFile pool fileID
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
            ior <- liftIO $ addDownload fp dls
            stream $ streamFromFile fp ior
        ServerError -> do
            status status500
            html "Something went wrong while looking up the file you requested."

--initiate a graceful shutdown of the server
shutdownHandler :: IORef Bool -> Socket -> ActionM ()
shutdownHandler shutdownIORef socket = do
    --here should be some sort of authentication, otherwise anyone can shut down the server
    --(not implemented yet however for this version)
    liftIO $ print "Shutting down..."
    --set the shutdownIORef to True. This tells the healthcheck function to start returning UNHEALTHY
    --responses to the load balancer. After 2 UNHEALTHY responses (default on GCE, can be altered),
    --the load balancer will conclude that the server is sick and won't route any new responses to it.
    --Existing conections are not terminated however.
    liftIO $ writeIORef shutdownIORef True
    --By default the health checks come every five seconds, so after a maximum of ten seconds there
    --should have been two failed health checks and this server will not get any new from the load
    --balancer anymore. Closing down the accepting socket  causes the warp server to gracefully
    --shut down after all outstanding requests have finished. We do this waiting and closing in a
    --separate thread, so that we can quickly respond to the request.
    liftIO . forkIO $ (threadDelay (10 * 1000 * 1000) >> sClose socket >> print "Closed accepting socket.")
    --return a response that you understood the request and everything went well
    text "OK"

--responds to health checks from the load balancer
healthCheckHandler :: IORef Bool -> ActionM ()
healthCheckHandler shutdownIORef = do
    shouldShutdown <- liftIO $ readIORef shutdownIORef
    if shouldShutdown
        then do
            status status404 --this is what actually matters, response body is just for clarity
            text "UNHEALTHY"
        else do
            --no need to change status, it's 200 by default anyway
            text "HEALTHY"

--returns the expected time to drain in seconds in the body of the response
--see the autoscaling module
drainTimeHandler :: DownloadStore -> ActionM()
drainTimeHandler dls = do
    edt <- liftIO $ getExpectedDrainTime dls
    text . TL.pack . show $ edt --size of edt is so low this will be fast even though it's inefficient

--returns the average load in the last few seconds 
loadHandler :: LoadState -> ActionM ()
loadHandler loadRef = do
    (_,_,rate) <- liftIO $ readIORef loadRef
    text . TL.pack . show $ rate