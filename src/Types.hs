{-# LANGUAGE OverloadedStrings #-}
module Types where 
--module for types and constants

import qualified Data.Text as T
import Data.Time

data RetrieveResult = ServerError | NotFound | Expired | TooManyDownloads | Found FilePath

uploadedFileDirectory :: T.Text
uploadedFileDirectory = "uploadedfiles/"

maxdownloads :: Int
maxdownloads = 5

maxage :: NominalDiffTime
maxage =  (7 * 24 * 60 * 60) --one week

dbpath :: FilePath
dbpath = "filedb.sqlite"

imageDirectory :: FilePath
imageDirectory = "backgrounds/"

vacuumInterval :: Int
vacuumInterval = 60 * 60 * 1000000 --one hour, in microseconds