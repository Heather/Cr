{-# LANGUAGE UnicodeSyntax #-}

module CommonDataStorage
  ( getLastVersionForPlatform
  , getChromium
  , getDart
  ) where

import Base

import System.Directory

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit

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
{------------------------- Last Chromium Version --------------------------------------}
getLastVersionForPlatform :: [Char] → IO String
getLastVersionForPlatform p = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" ++ p ++ "/LAST_CHANGE"
        in simpleHttp url
            >>= \bs → return $ S.decode $ L.unpack bs
{-------------------------  Chromium  --------------------------------------}
getChromium :: [Char] → [Char] → IO()
getChromium p v = withSocketsDo $ do
    let fname = "mini_installer.exe"
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                        ++ p ++ "/" ++ v ++ "/" ++ fname
    putStrLn $ " -> " ++ url
    irequest <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager → do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname
{----------------------------------------------------------------------------------------}
getDart :: [Char] → IO()
getDart p = case p of
    "Win" -> withSocketsDo $ do
        let tarball = "dartium-win.zip"
        let url = "http://storage.googleapis.com/dart-editor-archive-integration/latest/dartium-win.zip"
        irequest <- liftIO $ parseUrl url
        withManager $ \manager → do
            let request = irequest
                 { method = methodGet }
            response <- http request manager
            responseBody response C.$$+- sinkFile tarball
        let src = "dartium-win"
        let dst = "C:/dartium-win"
        dictZipFile <- B.readFile tarball
        extractFilesFromArchive [OptVerbose] $ toArchive dictZipFile
        srcExists <- doesDirectoryExist src
        dstExists <- doesDirectoryExist dst
        if or [not srcExists, dstExists] 
            then putStrLn " -> Can not copy to C:"
            else do
                copyDir src dst
                    >> removeDirectoryRecursive src
                    >> removeFile tarball
    _  -> putStrLn "This platform is not supported yet :("
{----------------------------------------------------------------------------------------}
