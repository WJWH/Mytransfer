{-# LANGUAGE OverloadedStrings #-}
module DatabaseCalls where

--A module that separates out all the database calls to make changing databases (more) easy.
--This is useful because the early versions used SQLite while I eventually want to change to Postgres
--to make autoscaling possible.

import Data.Time
import Database.SQLite.Simple

import Types

--used by the vacuum function in the Utilities module, will select all FIDs in the Files table that
--should be deleted, either because they have been downloaded or because they have been uploaded
--too long ago and have expired
getFidsToDelete :: Connection -> IO [Only FID]
getFidsToDelete conn = do
    now <- getCurrentTime
    query conn
        "SELECT id FROM Files WHERE (timesDownloaded >= ? OR timeOfUpload < ?) AND deletedYet = 0" 
        (maxdownloads, addUTCTime (negate maxage) now) --select all the FIDs that should be deleted

--Adds an uploaded file to the database so that its age and number of downloads can be tracked
addUploadedFile :: Connection -> FID -> IO ()
addUploadedFile conn fid = do
    now <- getCurrentTime
    execute conn 
        "INSERT INTO Files (id, timesDownloaded, timeOfUpload,deletedYet) VALUES (?,?,?,?)"
        (fid, 0 :: Int,now,False)

--Looks up how many times a file has been downloaded and when it has been uploaded
getFIDMetaData :: Connection -> FID -> IO [(Int,UTCTime)]
getFIDMetaData conn fid = query conn "SELECT timesDownloaded, timeOfUpload FROM Files WHERE id = ?" [fid] :: IO [(Int,UTCTime)]

--Increase the number of times that a file has been downloaded
increaseFileDownloads :: Connection -> FID -> IO ()
increaseFileDownloads conn fid = execute conn "UPDATE Files SET timesDownloaded = timesDownloaded+1 WHERE id = ?" [fid]