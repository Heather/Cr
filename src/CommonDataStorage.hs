module CommonDataStorage
  ( getLastVersionForPlatform
  , getChromium
  , getDart
  ) where

import System.IO
import System.Exit
import System.Directory
import System.Process

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit

import Data.List
import Data.Conduit.Binary (sinkFile)

import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B

import System.FilePath((</>))
{-------------------------------------------------------------------------------------}
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectory dst
    content <- getDirectoryContents src
    let xs = filter (`notElem` [".", ".."]) content
    forM_ xs $ \name -> let srcPath = src </> name
                            dstPath = dst </> name
        in doesDirectoryExist srcPath >>= \dirExist ->
            if dirExist then copyDir srcPath dstPath
                        else copyFile srcPath dstPath
{------------------------- Last Chromium Version --------------------------------------}
getLastVersionForPlatform :: [Char] -> IO String
getLastVersionForPlatform p = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                  ++ p 
                  ++ "/LAST_CHANGE"
        in simpleHttp url
            >>= \bs -> return $ S.decode $ L.unpack bs
{-------------------------  Chromium  --------------------------------------}
getChromium :: [Char] -> [Char] -> [Char] -> IO()
getChromium p v fname = withSocketsDo $ do
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
              ++ p ++ "/" 
              ++ v ++ "/" 
              ++ fname
    irequest <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname
{----------------------------------------------------------------------------------------}
getDart :: [Char] -> IO()
getDart p = withSocketsDo $ do
    let tarball = "dartium-win.zip"
        dst     = "C:/dartium-win"
        dartium = dst
                  ++ "\\chrome.exe"
        url     = "http://storage.googleapis.com/dart-editor-archive-integration/latest/" 
                  ++ tarball
    irequest <- liftIO $ parseUrl url
    putStrLn " -> Getting Dartium"
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile tarball
    dictZipFile <- B.readFile tarball
    putStrLn " -> Extracting"
    extractFilesFromArchive [OptVerbose] $ toArchive dictZipFile
    all <- getDirectoryContents "."
    let find = filter ("dartium-win-full-trunk" `isPrefixOf`) all
        len = length find
    case len of
        1 -> do
            let src = head find
            srcExists <- doesDirectoryExist src
            dstExists <- doesDirectoryExist dst
            putStrLn " -> Moving to C:\n"
            if srcExists
                then putStrLn $ " -> " ++ src ++ " is not directory"
                else do
                    when dstExists $ removeDirectoryRecursive dst
                    copyDir src dst >> removeDirectoryRecursive src
                    createProcess (proc dartium []) >> return ()
        _  ->   if len > 1
                    then putStrLn "there are already some extracted sources, please clean-up"
                    else putStrLn "can't find extracted sources"
{----------------------------------------------------------------------------------------}
