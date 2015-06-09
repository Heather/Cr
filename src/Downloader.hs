{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  , ScopedTypeVariables
  #-}

module Downloader
  ( getLastVersionForPlatform
  , getChromium
  ) where

import System.Directory
import Network.Socket
import Network.HTTP.Conduit
import Network.HTTP.Types

import Data.Conduit.Binary (sinkFile)

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad
import Control.Exception.Lifted
import Control.Monad.Trans.Resource
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

getLastVersionForPlatform ∷ String → IO String
getLastVersionForPlatform platform = withSocketsDo
    $ simpleHttp url >>= \bs → return ( S.decode $ L.unpack bs )
  where url = "http://storage.googleapis.com/chromium-browser-continuous/"
              ++ platform
              ++ "/LAST_CHANGE"

retryOnTimeout ∷ ResourceT IO a → ResourceT IO a
retryOnTimeout action = catch action $ \ (_ :: HttpException) → do
    liftIO $ putStrLn " -> Timed out. Trying again."
    liftIO $ threadDelay 2000000
    action

getChromium ∷ String → String → String → IO()
getChromium platform version fname = withSocketsDo $ do
    irequest  ← liftIO $ parseUrl url
    fileExist ← doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager → do
        let request = irequest
             { method = methodGet
             , responseTimeout = Just 10000000 }
        response ← retryOnTimeout ( http request manager )
        responseBody response C.$$+- sinkFile fname
  where url = "http://storage.googleapis.com/chromium-browser-continuous/"
              ++ platform ++ "/"
              ++ version ++ "/"
              ++ fname
