{-# LANGUAGE OverloadedStrings #-}
module DatabaseCalls where

--A module that separates out all the database calls to make changing databases (more) easy.
--This is useful because the early versions used SQLite while I eventually changed it to Postgres
--to make autoscaling possible.

import Control.Monad
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple

import Types

--Makes a connection pool to the database with one subpool, closing unused connections after 10 seconds and
--keeping open a maximum of 10 connections per subpool. These defaults should be fine.
makeDBConnectionPool :: IO (Pool Connection)
makeDBConnectionPool = createPool (connect postgresConnectionParameters) (close) 1 10 10


--used by the vacuum function in the Utilities module, will select all FIDs in the Files table that
--should be deleted, either because they have been downloaded or because they have been uploaded
--too long ago and have expired
getFidsToDelete :: Pool Connection -> IO [Only FID]
getFidsToDelete pool = withResource pool $ \conn -> do
    now <- getCurrentTime
    query conn
        "SELECT id FROM Files WHERE (timesDownloaded >= ? OR timeOfUpload < ?) AND NOT deletedYet" 
        (maxdownloads, addUTCTime (negate maxage) now) --select all the FIDs that should be deleted

--Adds an uploaded file to the database so that its age and number of downloads can be tracked
addUploadedFile :: Pool Connection -> FID -> IO ()
addUploadedFile pool fid = withResource pool $ \conn -> do
    now <- getCurrentTime
    void $ execute conn 
        "INSERT INTO Files (id, timesDownloaded, timeOfUpload,deletedYet) VALUES (?,?,?,?)"
        (fid, 0 :: Int,now,False)

--Looks up how many times a file has been downloaded and when it has been uploaded
getFIDMetaData :: Pool Connection -> FID -> IO [(Int,UTCTime)]
getFIDMetaData pool fid = withResource pool $ \conn -> query conn "SELECT timesDownloaded, timeOfUpload FROM Files WHERE id = ?" [fid] :: IO [(Int,UTCTime)]

--Increase the number of times that a file has been downloaded
increaseFileDownloads :: Pool Connection -> FID -> IO ()
increaseFileDownloads pool fid = withResource pool $ \conn -> void $ execute conn "UPDATE Files SET timesDownloaded = timesDownloaded+1 WHERE id = ?" [fid]

--Marks the file as deleted in the database
markFileAsDeleted :: DBConnectionPool -> FID -> IO ()
markFileAsDeleted pool fid = withResource pool $ \conn -> void $ execute conn "UPDATE Files SET deletedYet = TRUE WHERE id = ?" [fid]

--create table statement to create the Files table in sqlite:
--CREATE TABLE "Files" ("id" TEXT PRIMARY KEY  NOT NULL ,"timesDownloaded" INTEGER NOT NULL ,"timeOfUpload" DATETIME NOT NULL  DEFAULT (null) , "deletedYet" BOOL NOT NULL)
--create table statement to create the Files table in postgres:
--CREATE TABLE Files (id TEXT PRIMARY KEY  NOT NULL ,timesDownloaded INTEGER NOT NULL ,timeOfUpload TIMESTAMPTZ NOT NULL, deletedYet BOOL NOT NULL)