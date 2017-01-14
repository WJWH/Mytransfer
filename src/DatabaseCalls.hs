{-# LANGUAGE OverloadedStrings #-}
module DatabaseCalls where

--A module that separates out all the database calls to make changing databases (more) easy.
--This is useful because the early versions used SQLite while I eventually want to change to Postgres
--to make autoscaling possible.

import Data.Time
import Database.SQLite.Simple

import Types

getFidsToDelete conn = do
    now <- getCurrentTime
    query conn
        "SELECT id FROM Files WHERE (timesDownloaded >= ? OR timeOfUpload < ?) AND deletedYet = 0" 
        (maxdownloads, addUTCTime (negate maxage) now)
        :: IO [Only FID] --select all the FIDs that should be deleted

addUploadedFile :: Connection -> FID -> IO ()
addUploadedFile conn fid = do
    now <- getCurrentTime
    execute conn 
        "INSERT INTO Files (id, timesDownloaded, timeOfUpload,deletedYet) VALUES (?,?,?,?)"
        (fid, 0 :: Int,now,False)
            
getFIDMetaData :: Connection -> FID -> IO [(Int,UTCTime)]
getFIDMetaData conn fid = query conn "SELECT timesDownloaded, timeOfUpload FROM Files WHERE id = ?" [fid] :: IO [(Int,UTCTime)]

increaseFileDownloads :: Connection -> FID -> IO ()
increaseFileDownloads conn fid = execute conn "UPDATE Files SET timesDownloaded = timesDownloaded+1 WHERE id = ?" [fid]