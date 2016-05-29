{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Downloader
  ( getLastVersionForPlatform
  , getChromium
  ) where

import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.Socket
import           System.Directory

import           Data.Conduit.Binary          (sinkFile)

import qualified Codec.Binary.UTF8.String     as S
import qualified Data.ByteString.Lazy         as L
import qualified Data.Conduit                 as C

import           Control.Concurrent
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

urlbase ∷ String
urlbase = "https://www.googleapis.com/download/storage/v1/b/chromium-browser-snapshots/o/"

infixl 2 <+>

(<+>) ∷ String → String → String
a <+> b = a ++ "%2F" ++ b

getUrl ∷ String → String → String
getUrl τ σ = urlbase ++ τ <+> σ ++ "?alt=media"

getLastVersionForPlatform ∷ String → IO String
getLastVersionForPlatform τ = withSocketsDo
    $ simpleHttp url >>= \bs → return ( S.decode $ L.unpack bs )
  where url = getUrl τ "LAST_CHANGE"

retryOnTimeout ∷ ResourceT IO α → ResourceT IO α
retryOnTimeout action = catch action $ \ (_ :: HttpException) → do
    liftIO $ putStrLn " -> Timed out. Trying again."
    liftIO $ threadDelay 2000000
    action

getChromium ∷ String → String → String → IO()
getChromium τ version fname = withSocketsDo $ do
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
        ρ ← retryOnTimeout ( http request manager )
        responseBody ρ C.$$+- sinkFile fname
  where url = getUrl τ (version <+> fname)
