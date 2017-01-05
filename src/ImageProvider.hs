module ImageProvider where

--    ( getImage
--    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Directory

imageDirectory = "backgrounds/"

--interface to the main app. It is intended to be easily refactored for more arguments
--when I want to select a background based on screen size or whatnot.
--returns a function that returns a filepath
startBackgroundProvider :: IO (IO FilePath)
startBackgroundProvider = do
    mv <- newEmptyMVar
    forkIO $ backgroundProvider mv
    return (takeMVar mv)

--Makes sure that the supplied MVar is "always" filled with a filename to a background image
--Currently serves images in a round robin fashion from a directory full of nice images
backgroundProvider :: MVar FilePath -> IO ()
backgroundProvider mv = do
    filenames <- cycle . (map (imageDirectory ++ )) <$> listDirectory imageDirectory --infinite list of filenames
    forM_ filenames $ \f -> putMVar mv f --put filename in MVar, when someone empties the MVar, put another one in
    --because filenames is an infinite list, this loops forever, laziness makes sure that memory usage
    --stays under control