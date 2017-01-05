module StorageBackend where

import qualified Data.ByteString.Lazy as BL
import Network.Wai.Parse

--there are four main functions:
-- add a file
-- retrieve a file
-- check if a certain file exists
-- delete a file (if it exists)

--this backend uses the local file system rather than an object storage service like S3
--however, if needed it should be relatively straightforward to change this later due to the 
--design of the module interface

addFile :: 