{-# LANGUAGE OverloadedStrings #-}
module StorageBackend where

import Control.Applicative
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID
import Database.SQLite.Simple
import Network.Wai.Parse
import System.Random

--there are four main functions:
--  add a file
--  retrieve a file
--  check if a certain file exists
--  delete a file (if it exists)
--Of these, only the first two are accessible by the user, deleting happens automatically though a 
--script after <maxdownloads> downloads or after <maxage> seconds have passed.


--this backend uses the local file system rather than an object storage service like S3
--however, if needed it should be relatively straightforward to change this later due to the
--design of the module interface

--we don't keep files forever, they can be downloaded a maximum of <maxdownloads> times
--currently a sqlite database is used for this, this can later be changed to an online database

uploadedFileDirectory = "uploadedfiles/"
maxdownloads = 5
maxage =  (7 * 24 * 60 * 60) --one week
dbpath = "filedb.sqlite"

data RetrieveResult = ServerError | NotFound | Expired | TooManyDownloads | Found FilePath

--writes an uploaded file to disk
--adds a UUID to the filename to distinguish it from other files with the same name
--returns the actual filenames on disk so they can be mailed to the user
addFile :: FileInfo BL.ByteString -> IO T.Text
addFile (FileInfo fileName fileType fileContents) = do
    --add a UUID to the file so files with the same name don't get overwritten
    uuidedFilename <- TE.decodeLatin1 . (\u ->"mytransfer-" <> u <> "-" <> fileName) . toASCIIBytes <$> randomIO
    --write the file
    BL.writeFile (T.unpack $ uploadedFileDirectory <> uuidedFilename) fileContents
    --register the file in the database, initially it obviously has zero downloads
    now <- getCurrentTime
    withConnection dbpath $ \conn -> do
        execute conn "INSERT INTO Files (id, timesDownloaded, timeOfUpload) VALUES (?,?,?)" (uuidedFilename, 0 :: Int,now)
    return uuidedFilename

--this one does not do very much for the disk based version, in a version with an object storage
--service this will become more involved.
--it receives the file ID and checks if it exists and hasn't been downloaded <maxdownloads> times yet
--if that is true, it unpacks the file ID and returns it so that it can be served to the user
--bonus difficulty: what to do if this is the final time the file can be downloaded?
--currently it leaves the file on the disk, an hourly cron job cleans up files
retrieveFile :: T.Text -> IO RetrieveResult
retrieveFile fid = withConnection dbpath $ \conn -> do
    rs <- query conn "SELECT timesDownloaded, timeOfUpload FROM Files WHERE id = ?" [fid] :: IO [(Int,UTCTime)]
    now <- getCurrentTime
    case rs of
        [] -> return NotFound --file is not known to the database
        [(numdownloads, timeOfUpload)] -> do
            if numdownloads < maxdownloads
                then if (now < (addUTCTime maxage timeOfUpload))
                        then do --file is not yet expired
                            --update timesDownloaded in the db
                            execute conn "UPDATE Files SET timesDownloaded = timesDownloaded+1 WHERE id = ?" [fid]
                            --return the unpacked filepath
                            return $ Found (T.unpack $ uploadedFileDirectory <> fid)
                        else do --file has been uploaded too long ago
                            return Expired
                else return TooManyDownloads -- downloaded too many times already
        _ -> return ServerError --because id is the primary key in the table, so it must be unique
