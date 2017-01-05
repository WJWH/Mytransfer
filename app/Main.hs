{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import ImageProvider

main :: IO ()
main = do
    getBackgroundPath <- startBackgroundProvider --returns a function that returns a image filepath
    scotty 3000 $ do
        middleware logStdoutDev
        get "/" showLandingPage  
        get "/background" $ serveBackground getBackgroundPath
        post "/upload" uploadFile
        --post "/download"
        
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
    pars <- params
    liftIO $ mapM_ print pars 
    fs <- files
    liftIO $ mapM_ print fs
    status status204