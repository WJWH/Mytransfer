{-# LANGUAGE OverloadedStrings #-}
module StorageBackend where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID
import Database.SQLite.Simple
import Network.Wai.Parse
import System.Directory
import System.Exit
import System.Process
import System.Random


import Types

--there are four main functions:
--  add a file
--  retrieve a file
--  check if a certain file exists
--  delete a file (if it exists)
--Of these, only the first two are accessible by the user, deleting happens automatically though a
--the vacuum function in Utilities after <maxdownloads> downloads or after <maxage> seconds have
--passed. Checking if a file exist is currently a part of retrieveFile as it's not used anywhere else.


--this backend uses the local file system rather than an object storage service like S3
--however, if needed it should be relatively straightforward to change this later due to the
--design of the module interface

--we don't keep files forever, they can be downloaded a maximum of <maxdownloads> times
--currently a sqlite database is used for this, this can later be changed to an online database


--writes an uploaded file to disk
--adds a UUID to the filename to distinguish it from other files with the same name
--returns the actual filenames on disk so they can be mailed to the user
addFile :: FileInfo BL.ByteString -> IO FID
addFile (FileInfo fn _ fileContents) = do
    --add a UUID to the file so files with the same name don't get overwritten
    uuidedFilename <- TE.decodeLatin1 . (\u ->"mytransfer-" <> u <> "-" <> fn) . toASCIIBytes <$> randomIO
    --write the file to a temporary local location
    BL.writeFile (T.unpack $ uploadedFileDirectory <> uuidedFilename) fileContents
    --use a background thread to upload the file to Google Cloud Storage and then delete it
    forkIO $ uploadFile uuidedFilename
    --register the file in the database, initially it obviously has zero downloads
    now <- getCurrentTime
    withConnection dbpath $ \conn -> do
        execute conn "INSERT INTO Files (id, timesDownloaded, timeOfUpload,deletedYet) VALUES (?,?,?,?)" (uuidedFilename, 0 :: Int,now,False)
    return uuidedFilename

--Background thread function to upload a file to GCS and then delete it from local storage
--it forks out to the gsutil program to do the actual uploading
uploadFile :: FID -> IO ()
uploadFile fid = do
    uploadResult <- system . T.unpack $ "gsutil cp " <> uploadedFileDirectory <> fid <> "gs://" <> gcsBucketName
    case uploadResult of
        ExitSuccess -> removeFile (T.unpack $ uploadedFileDirectory <> fid) --this means the file is safely uploaded so we can delete the local copy
        _ -> return () -- uploading failed, leave the file in the local directory and we will retry later

--this one does not do very much for the disk based version, in a version with an object storage
--service this will become more involved.
--it receives the file ID and checks if it exists and hasn't been downloaded <maxdownloads> times yet
--if that is true, it unpacks the file ID and returns it so that it can be served to the user
--bonus difficulty: what to do if this is the final time the file can be downloaded?
--currently it leaves the file on the disk, an hourly cron job cleans up files
retrieveFile :: FID -> IO RetrieveResult
retrieveFile fid = withConnection dbpath $ \conn -> do
    rs <- query conn "SELECT timesDownloaded, timeOfUpload FROM Files WHERE id = ?" [fid] :: IO [(Int,UTCTime)]
    now <- getCurrentTime
    case rs of
        [] -> return NotFound --file is not known to the database
        [(numdownloads, timeOfUpload)] -> do
            if numdownloads < maxdownloads
                then if (now < (addUTCTime maxage timeOfUpload))
                        then do --file is not yet expired
                            --retrieve the file from GCS, inline since it sadly makes no sense to do
                            --this in a background thread
                            system . T.unpack $ "gsutil cp gs://" <> gcsBucketName <> fid <> " " <> downloadedFileDirectory <> fid
                            deleteDownloadedFileAfterFiveSeconds fid --see function for why this is safe
                            --update timesDownloaded in the db
                            execute conn "UPDATE Files SET timesDownloaded = timesDownloaded+1 WHERE id = ?" [fid]
                            --return the unpacked filepath
                            return $ Found (T.unpack $ uploadedFileDirectory <> fid)
                        else do --file has been uploaded too long ago
                            return Expired
                else return TooManyDownloads -- downloaded too many times already
        _ -> return ServerError
        --id is the primary key in the table, so it must be unique
        --if there are multiple results when selecting on it there must be an error

--through a stroke of luck, the standard library function to delete files is called "removeFile", so we can
--use "deleteFile" as our function name. This function tries to delete a file from the backing store and
--returns a Bool indicating whether the operation was succesful or not.
deleteFile :: FID -> IO Bool
deleteFile fid = do
    deleteResult <- system . T.unpack $ "gsutil rm gs://" <> gcsBucketName <> fid
    case deleteResult of
        ExitSuccess -> return True
        _ -> return False

--this will delete the named file from the local filesystem after five seconds, so that it does not clog
--up the local disk. We wait for five seconds so we can reasonably assume that the server has opened the 
--file and is now serving it to the user (or even has finished already. The filesystem will unlink the
--filename, but won't actually delete the contents (the inode) until all file descriptors to it have been
--closed. Therefore, as soon as the server is done with it and closes the file descriptor, the inode will
--be deleted.
deleteDownloadedFileAfterFiveSeconds :: FID -> IO ()
deleteDownloadedFileAfterFiveSeconds fid = do
    threadDelay 5000000
    removeFile . T.unpack $ downloadedFileDirectory <> fid
