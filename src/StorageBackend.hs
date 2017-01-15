{-# LANGUAGE OverloadedStrings #-}
module StorageBackend where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID
import Network.Wai.Parse
import System.Directory
import System.Exit
import System.Process
import System.Random

import DatabaseCalls
import Types

--there are three main functions:
--  add a file
--  retrieve a file
--  delete a file (if it exists)
--Of these, only the first two are accessible by the user, deleting happens automatically though a
--the vacuum function in Utilities after <maxdownloads> downloads or after <maxage> seconds have
--passed. Checking if a file exists is currently a part of retrieveFile as it's not used anywhere else.

--this backend is based on gcsfuse, which is a way to mount a Google Cloud Storage (GCS) bucket
--as a local file system. This makes it possible for all servers to access the same files. Since each
--file/object in the bucket has a unique name (due to using UUIDs), there is no danger that servers will
--accidentally overwrite each others files and they can all have the bucket mounted safely.

--we don't keep files forever, they can be downloaded a maximum of <maxdownloads> times, or after
-- <maxage> time has passed.
--originally a sqlite database was used for tracking this, this has since been changed to a postgres db


--writes an uploaded file to the GCS bucket through the gcsfuse filesystem
--adds a UUID to the filename to distinguish it from other files with the same name
--returns the actual filenames on disk so they can be mailed to the user
addFile :: DBConnectionPool -> FileInfo BL.ByteString -> IO FID
addFile pool (FileInfo fn _ fileContents) = do
    --add a UUID to the file so files with the same name don't get overwritten
    fid <- TE.decodeLatin1 . (\u ->"mytransfer-" <> u <> "-" <> fn) . toASCIIBytes <$> randomIO
    --write the file to the bucket
    BL.writeFile (T.unpack $ uploadedFileDirectory <> fid) fileContents
    --register the file in the database, initially it obviously has zero downloads
    addUploadedFile pool fid
    return fid

--this one does not do very much for the disk based version, in a version with an object storage
--service this will become more involved.
--it receives the file ID and checks if it exists and hasn't been downloaded <maxdownloads> times yet
--if that is true, it unpacks the file ID and returns it so that it can be served to the user
--bonus difficulty: what to do if this is the final time the file can be downloaded?
--currently it leaves the file on the disk, an hourly cron job cleans up files
retrieveFile :: DBConnectionPool -> FID -> IO RetrieveResult
retrieveFile pool fid = do
    rs <- getFIDMetaData pool fid
    now <- getCurrentTime
    case rs of
        [] -> return NotFound --file is not known to the database
        [(numdownloads, timeOfUpload)] -> do
            if numdownloads < maxdownloads
                then if (now < (addUTCTime maxage timeOfUpload))
                    then do --file is not yet expired
                        --update timesDownloaded in the db
                        increaseFileDownloads pool fid
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
    deleteResult <- try (removeFile . T.unpack $ uploadedFileDirectory <> fid) :: IO (Either IOException ())
    case deleteResult of
        Right () -> return True
        Left _ -> return False

