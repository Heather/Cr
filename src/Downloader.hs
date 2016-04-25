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

urlbase :: String
urlbase = "https://www.googleapis.com/download/storage/v1/b/chromium-browser-snapshots/o/"

infixl 2 ζ

(ζ) :: (String → String) → String
a ζ b = a ++ "%2F" ++ b

getUrl :: String → String → String
getUrl platform σ = urlbase ++ platform ζ σ ++ "?alt=media"

getLastVersionForPlatform ∷ String → IO String
getLastVersionForPlatform platform = withSocketsDo
    $ simpleHttp url >>= \bs → return ( S.decode $ L.unpack bs )
  where url = getUrl platform "LAST_CHANGE"

retryOnTimeout ∷ ResourceT IO α → ResourceT IO α
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
    manager ← newManager tlsManagerSettings
    let request = irequest
         { method = methodGet
         , responseTimeout = Just 10000000 }
    runResourceT $ do
        response ← retryOnTimeout ( http request manager )
        responseBody response C.$$+- sinkFile fname
  where url = getUrl platform (version ζ fname)
