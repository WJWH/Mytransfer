module Bib
    ( someFunc
    ) where

import System.Directory

someFunc :: IO ()
someFunc = do
    here <- getCurrentDirectory
    putStrLn here
