{-# LANGUAGE OverloadedStrings #-}
module Types where 
--module for types and constants

import qualified Data.Text as T
import Data.Time

data RetrieveResult = ServerError | NotFound | Expired | TooManyDownloads | Found FilePath

uploadedFileDirectory = "uploadedfiles/" :: T.Text
maxdownloads = 5 :: Int
maxage =  (7 * 24 * 60 * 60) :: NominalDiffTime --one week
dbpath = "filedb.sqlite" :: FilePath
imageDirectory = "backgrounds/"
vacuumInterval = 60 * 60 * 1000000 :: Int --one hour, in microseconds