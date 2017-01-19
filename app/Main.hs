{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.IORef
import Network
import Network.Wai.Handler.Warp (defaultSettings)
import Web.Scotty

import ImageProvider
import StorageBackend
import Mailer
import Types
import Utilities
import Autoscaling
import DatabaseCalls
import MainApp

scottyopts = Options 0 defaultSettings

main :: IO ()
main = do
    void $ assertStartupEnvironment --checks if all necessary directories and db's are present
    pool <- makeDBConnectionPool --start a pool of database connections
    startVacuumThread pool --starts the vacuumer that will clean up old files
    getBackgroundPath <- startBackgroundProvider --returns a function that returns a image filepath
    (dls,loadRef) <- initDownloadTracker --create a DownloadStore and a periodic cleaning thread for it
    shutdownIORef <- newIORef False
    sock <- listenOn $ PortNumber 80
    --app is defined in MainApp
    scottySocket scottyopts sock $ app getBackgroundPath pool dls shutdownIORef sock loadRef
    closeConnections pool --close all the db connections