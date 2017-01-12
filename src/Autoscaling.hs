{-# LANGUAGE OverloadedStrings #-}
module Autoscaling where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Data.Time
import System.IO

--Observation: For a service such as this one, autoscaling is a little bit more involved than for most web services, because some requests might take super long to complete. Take for instance a user with very low bandwidth trying to download a large file.

--Observation: fine control of autoscaling is apparently quite tricky on AWS and GCE, you can only very coarsely control which instance will be deactivated when downscaling. OldestFirst or YoungestFirst is already very advanced. Autoscaling based on which instance publishes the lowest value on a HTTP endpoint is definately a bridge too far.

--Oplossing: Use a load balancer in front of an instance group that balances the requests round robin over the (healthy) instances in the group. The load balancer is NOT responsible for autoscaling, that is done by a separate program monitoring all the servers.

--This module implements a few functions to make this work on the server end:
-- Download is a datastructure representing a download in progress
-- addDownload registers a new download for tracking
-- getExpectedDrainTime returns the expected number of seconds in which all CURRENT connections will finish
-- cleanDrainList gets run periodically to clean up finished downloads 
-- initDownloadTracker sets the whole thing up

data Download = DL  { filesize :: Int --maxBound :: Int shows it's enough up to about 9 Exabytes
                    , progressIORef :: IORef Int
                    , timeStarted :: UTCTime 
                    } deriving (Eq)

--For now a list will do fine. Using a MVar to prevent race conditions in adding and deleting entries
type DownloadStore = MVar [Download]

--creates a DownloadStore, starts a vacuuming thread 
initDownloadTracker :: IO DownloadStore
initDownloadTracker = do
    dls <- newMVar []
    forkIO $ vacuumer dls
    return dls

--given a filepath and a DownloadStore, starts tracking it. returns a IORef to be used in the counting conduit
addDownload :: FilePath -> DownloadStore -> IO (IORef Int)
addDownload fp dls = do 
    fs <- fromInteger <$> (withFile fp ReadMode $ \hdl -> hFileSize hdl)
    ior <- newIORef 0 :: IO (IORef Int)
    now <- getCurrentTime
    modifyMVar_ dls $ (\downloads -> return $ (DL fs ior now):downloads)
    return ior

--for a single Download, compute average speed so far and use that to estimate how long the rest of the download will take. This assumes that the average download speed for any user won't change significantly over time.
computeExpectedCompletionTime :: Download -> IO Int
computeExpectedCompletionTime dl = do
    progress <- readIORef $ progressIORef dl
    now <- getCurrentTime
    --writing it out for clarity
    let averageSpeedSoFar = progress `div` (ceiling (diffUTCTime now (timeStarted dl))) -- integral division, but should be fine as the error in estimation will dwarf this conversion error
    let bytesLeft = (filesize dl) - progress
    let expectedTimeLeft = if averageSpeedSoFar > 0 then bytesLeft `div` averageSpeedSoFar else maxBound --see previous comment about integer division. Also, if the progress was zero, then averageSpeedSofar must be zero. We set the result the maxBound :: Int to avoid an exception.
    return expectedTimeLeft

--the expected time for all connections to drain is the maximum of each individual connection
getExpectedDrainTime :: DownloadStore -> IO Int
getExpectedDrainTime dls = do
    downloads <- readMVar dls 
    completionTimes <- mapM computeExpectedCompletionTime downloads
    return $ maximum completionTimes

--rums every hour to clean up the list of downloads, since you don't have to track downloads that have completed 
vacuumer :: DownloadStore->  IO ()
vacuumer dls = forever $ do
    threadDelay $ 60 * 60 * 1000000 --wait one hour
    forkIO $ cleanDownloads dls --run in an separate thread so 
    
cleanDownloads :: DownloadStore -> IO ()
cleanDownloads dls = do
    downloads <- takeMVar dls --this will block every thread trying to use the MVar, unavoidable
    activeDownloads <- filterM isNotYetCompleted downloads --filter out completed downloads
    putMVar dls activeDownloads --the downloads that have not completed yet are put back in the DownloadStore
    
isNotYetCompleted :: Download -> IO Bool
isNotYetCompleted dl = do
    progress <- readIORef $ progressIORef dl
    return $ progress == (filesize dl) --if the number of bytes sent is the number of bytes in the file, the download is complete