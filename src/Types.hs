{-# LANGUAGE OverloadedStrings #-}
module Types where 
--module for types and constants

import qualified Data.Text as T
import Data.Time

-- A FID is the T.Text version of a filename or an object name. It consists of the
--word "mytransfer-", then a random UUID , another "-" and then the original filename
--as uploaded by the user. They are created in the addFile function in StorageBackend
type FID = T.Text

data RetrieveResult = ServerError | NotFound | Expired | TooManyDownloads | Found FilePath

--name of the bucket in Google Cloud Storage where all the user files are stored
--don't forget the / at the end!
gcsBucketName :: T.Text
gcsBucketName = "mytransfer-files/"

--uploaded files are kept here before they are uploaded, then they are deleted
uploadedFileDirectory :: T.Text
uploadedFileDirectory = "uploadedfiles/"

--after this many downloads, the vacuumer will delete the file from storage
maxdownloads :: Int
maxdownloads = 5

--after this many seconds, the vacuumer will delete the file from storage
maxage :: NominalDiffTime
maxage =  (7 * 24 * 60 * 60) --one week

--the place where the SQLite database storing information about the files is stored
dbpath :: FilePath
dbpath = "filedb.sqlite"

--the directory where images to be used as backgrounds are stored
imageDirectory :: FilePath
imageDirectory = "backgrounds/"

--the interval at which the vacuum function is called
vacuumInterval :: Int
vacuumInterval = 60 * 60 * 1000000 --one hour, in microseconds