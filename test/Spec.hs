{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import           Control.Monad
import           Data.IORef
import           Network
import           Network.Wai (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Web.Scotty as S

import ImageProvider
import StorageBackend
import Mailer
import Types
import Utilities
import Autoscaling
import DatabaseCalls
import MainApp

--this is the main entry point for all the tests
main :: IO ()
main = hspec spec

myapp :: IO Application
myapp = do
    void $ assertStartupEnvironment --checks if all necessary directories and db's are present
    pool <- makeDBConnectionPool --start a pool of database connections
    startVacuumThread pool --starts the vacuumer that will clean up old files
    getBackgroundPath <- startBackgroundProvider --returns a function that returns a image filepath
    (dls,loadRef) <- initDownloadTracker --create a DownloadStore and a periodic cleaning thread for it
    shutdownIORef <- newIORef False
    --sock <- listenOn $ PortNumber 3001 --different port for testing
    S.scottyApp $ app getBackgroundPath pool dls shutdownIORef undefined loadRef

spec :: Spec
spec = with myapp $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "has 'Content-Type: text/html'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/html"]}

--  describe "GET /some-json" $ do
--    it "responds with some JSON" $ do
--      get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]