{-# LANGUAGE OverloadedStrings #-}
module Utilities where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import System.Directory

import StorageBackend
import Types

--little function that will check whether aal the preconditions for the server are present
--examples: database must be present, directory to store uploaded files must be present, etc
assertStartupEnvironment :: IO Bool
assertStartupEnvironment = do
    dbExists <- doesFileExist dbpath --dbpath is defined in Types
    unless dbExists (error "Could not find filedb.sqlite, it must be present in the same directory as the Mytransfer executable.")
    uploadDirExists <- doesDirectoryExist (T.unpack uploadedFileDirectory)
    unless uploadDirExists (error "Could not find directory uploadedfiles, it must be present as a subdirectory to the directory containing the Mytransfer executable.")
    backgroundsDirExists <- doesDirectoryExist imageDirectory
    unless backgroundsDirExists (error "Could not find directory backgrounds, it must be present as a subdirectory to the directory containing the Mytransfer executable.")
    print $ "Environment seems good, starting..." 
    return True
    
--starts the vacuum thread
startVacuumThread :: IO ()
startVacuumThread = void $ forkIO vacuumThread 

--this runs the vacuum function once per <vacuuminterval> microseconds
--it runs the vacuum function in a separate thread so that if it crashes it does not take this thread with it
vacuumThread :: IO ()
vacuumThread = forever $ do
    forkIO vacuum
    threadDelay vacuumInterval

vacuum :: IO ()
vacuum = withConnection dbpath $ \conn -> do
    now <- getCurrentTime
    print $ "Started vacuumer at " ++ (show now) 
    fidsToDelete <- query conn
        "SELECT id FROM Files WHERE (timesDownloaded >= ? OR timeOfUpload < ?) AND deletedYet = 0" 
        (maxdownloads, addUTCTime (negate maxage) now)
        :: IO [Only FID] --select all the FIDs that should be deleted
    forM_ fidsToDelete $ \(Only fid) -> do
        succeeded <- deleteFile fid
        when succeeded $ execute conn "UPDATE Files SET deletedYet = 1 WHERE id = ?" [fid]
        print $ "Vacuumed file: " ++ (T.unpack fid) ++ ", succesfully removed: " ++ (show succeeded)
