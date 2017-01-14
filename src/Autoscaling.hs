{-# LANGUAGE OverloadedStrings #-}
module Autoscaling where

import Control.Concurrent hiding (yield)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.IORef
import Data.List
import Data.Time
import Network.Wai
import System.IO

import Types

--Observation: For a service such as this one, autoscaling is a little bit more involved than for most web services, because some requests might take super long to complete. Take for instance a user with very low bandwidth trying to download a large file.

--Observation: fine control of autoscaling is apparently quite tricky on AWS and GCE, you can only very coarsely control which instance will be deactivated when downscaling. OldestFirst or YoungestFirst is already very advanced. Autoscaling based on which instance publishes the lowest value on a HTTP endpoint is definately a bridge too far.

--Oplossing: Use a load balancer in front of an instance group that balances the requests round robin over the (healthy) instances in the group. The load balancer is NOT responsible for autoscaling, that is done by a separate program monitoring all the servers.

--This module implements a few functions to make this work on the server end:
-- Download is a datastructure representing a download in progress
-- addDownload registers a new download for tracking
-- getExpectedDrainTime returns the expected number of seconds in which all CURRENT connections will finish
-- cleanDrainList gets run periodically to clean up finished downloads
-- initDownloadTracker sets the whole thing up



--creates a DownloadStore, starts a vacuuming thread
initDownloadTracker :: IO (DownloadStore, LoadState)
initDownloadTracker = do
    dls <- newMVar []
    now <- getCurrentTime
    ls <- newIORef (now,0,0)
    forkIO $ vacuumer dls ls
    forkIO $ loadCalculator ls dls 
    return (dls,ls)

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
    return $ safeMaximum completionTimes

--the normal maximum function returns an error on an empty list, this one returns zero which is a much
--saner default in this case
safeMaximum = foldl' (+) 0

--rums every hour to clean up the list of downloads, since you don't have to track downloads that have completed
vacuumer :: DownloadStore -> LoadState -> IO ()
vacuumer dls ls = forever $ do
    threadDelay $ 60 * 60 * 1000000 --wait one hour
    forkIO $ cleanDownloads dls ls --run in an separate thread so

--delete all the fininshed downloads from the DownloadStore, then upload the LoadState
cleanDownloads :: DownloadStore -> LoadState -> IO ()
cleanDownloads dls loadRef = do
    downloads <- takeMVar dls --this will block every thread trying to use the MVar, unavoidable
    activeDownloads <- filterM isNotYetCompleted downloads --filter out completed downloads
    putMVar dls activeDownloads --the downloads that have not completed yet are put back in the DownloadStore
    --the MVar is now unblocked and can be used again
    --now we update the load state so it won't give negative load due to all the completed files
    --disappearing from the progress calculation. We keep the average load from last time.
    (prevRunTime, prevTotalBytes, lastRate) <- readIORef loadRef
    totalBytesNow <- sum <$> mapM (\d -> readIORef $ progressIORef d) activeDownloads
    now <- getCurrentTime
    writeIORef loadRef (now,totalBytesNow,lastRate)

isNotYetCompleted :: Download -> IO Bool
isNotYetCompleted dl = do
    progress <- readIORef $ progressIORef dl
    return $ progress == (filesize dl) --if the number of bytes sent is the number of bytes in the file, the download is complete

--streams a file through the Scotty "stream" function
streamFromFile :: FilePath -> IORef Int -> StreamingBody
streamFromFile fp ior w f = runConduitRes $ C.sourceFileBS fp =$= countingConduit ior =$= (streamingConduit w f)

--streams an uploaded string of bytes into a file on a local filesystem
streamToFile :: FilePath -> BL.ByteString -> IORef Int -> IO ()
streamToFile fp bs ior = runConduitRes $ C.sourceLazy bs =$= countingConduit ior =$= C.sinkFile fp

--counts the number of bytes flowing through it and stores this in the IORef provided as an argument
countingConduit :: MonadIO m => IORef Int -> ConduitM B.ByteString B.ByteString m ()
countingConduit ior = do
    bs <- await
    case bs of
        Just bs' -> do
            --use modifyIORef' here instead of modifyIORef!!! space leak otherwise due to building up thunks
            liftIO $ modifyIORef' ior (+ (B.length bs'))
            liftIO (readIORef ior >>= print)
            yield bs'
            countingConduit ior
        Nothing -> return ()

--using the streaming functions of a StreamingBody to stream the incoming bytes from the Conduit
streamingConduit :: MonadIO m => (BB.Builder -> IO a) -> IO a1 -> ConduitM B.ByteString o m ()
streamingConduit write flush = do
    bs <- await
    case bs of
        Just bs' -> do
            liftIO $ write (BBE.byteStringInsert bs')
            liftIO $ flush
            streamingConduit write flush
        Nothing -> return ()

--runs calculateLoad every <loadCalculationRate> seconds
loadCalculator :: LoadState -> DownloadStore -> IO ()
loadCalculator loadRef dls = forever $ do
    threadDelay loadCalculationRate
    forkIO $ calculateLoad loadRef dls

--calculates the load over the last few seconds and puts the result in the IOVar
--might be slightly off the "real" value due to race conditions but that should average out
calculateLoad loadRef dls = do
    (prevRunTime, prevTotalBytes, _) <- readIORef loadRef
    downloads <- readMVar dls
    totalBytes <- sum <$> mapM (\d -> readIORef $ progressIORef d) downloads
    now <- getCurrentTime
    --written out for clarity
    let bytesSinceLastRun = totalBytes - prevTotalBytes
    let timePassedSinceLastRun = ceiling (diffUTCTime now (prevRunTime))
    let averageLoad = bytesSinceLastRun `div` timePassedSinceLastRun
    writeIORef loadRef (now,totalBytes,averageLoad) --write the IORef with the updated values

