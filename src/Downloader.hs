{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Downloader
  ( getLastVersionForPlatform
  , getChromium
  ) where

import System.IO
import System.Directory

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

getLastVersionForPlatform ∷ String → IO String
getLastVersionForPlatform platform = withSocketsDo
    $ simpleHttp url >>= \bs → return ( S.decode $ L.unpack bs )
  where url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
              ++ platform
              ++ "/LAST_CHANGE"

getChromium ∷ String → String → String → IO()
getChromium platform version fname = withSocketsDo $ do
    irequest  ← liftIO $ parseUrl url
    fileExist ← doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager → do
        let request = irequest
             { method = methodGet }
        response ← http request manager
        responseBody response C.$$+- sinkFile fname
  where url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
              ++ platform ++ "/" 
              ++ version ++ "/" 
              ++ fname
