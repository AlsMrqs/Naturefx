module Spirit.Soul where

import Control.Concurrent (forkIO,threadDelay)

import System.Directory (removeFile,renameFile,doesFileExist)
import System.Process (system)
import System.IO (hClose,hPutStr,openTempFile)

import Data.Bool (bool)
    
newSpeed :: (Int -> Int) -> FilePath -> IO ()
newSpeed f str = do
    (newFile,handle) <- openTempFile "." "temp"
    readFile str >>= hPutStr handle . show . f . read
    hClose handle
    removeFile str
    renameFile newFile str

acelerate :: FilePath -> IO ()
acelerate = newSpeed succ

desacelerate :: FilePath -> IO ()
desacelerate str = do 
    --threadDelay 1000000
    threadDelay 100000
    already <- doesFileExist str
    if not already then desacelerate str
    else do
        readFile str >>= bool (newSpeed pred str) (return ()) . (==) "0"
        desacelerate str


showSpeed :: FilePath -> IO ()
showSpeed str = do 
    threadDelay 100000
    already <- doesFileExist str
    if not already then showSpeed str
    else do
        system "clear"
        readFile str >>= putStrLn . (++) "down: "
        showSpeed str

