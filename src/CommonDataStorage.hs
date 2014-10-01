module CommonDataStorage
  ( getLastVersionForPlatform
  , getChromium
  , getDartium
  , getFF
  ) where

import System.IO
import System.Exit
import System.Directory
import System.Process

import Network.Socket
import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Types

import Data.List
import Data.Conduit.Binary (sinkFile)

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad
import Control.Monad.IO.Class (liftIO)

getLastVersionForPlatform :: [Char] -> IO String
getLastVersionForPlatform platform = withSocketsDo
    $ let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                  ++ platform
                  ++ "/LAST_CHANGE"
      in simpleHttp url >>= \bs -> return ( S.decode $ L.unpack bs )

getChromium :: [Char] -> [Char] -> [Char] -> IO()
getChromium platform version fname = withSocketsDo $ do
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
              ++ platform ++ "/" 
              ++ version ++ "/" 
              ++ fname
    irequest  <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname

getDartium :: [Char] -> IO()
getDartium fname = withSocketsDo $ do
    let url = "http://storage.googleapis.com/dart-archive/channels/dev/release/latest/dartium/"
              ++ fname
    irequest  <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname

getFF :: [Char] -> IO()
getFF fname = withSocketsDo $ do
    let url = "http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-trunk/"
              ++ fname
    irequest  <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname
