{-# LANGUAGE UnicodeSyntax #-}

module CommonDataStorage
  ( getLastVersionForPlatform
  , getChromium
  ) where

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad.IO.Class (liftIO)
{------------------------- Last Chromium Version --------------------------------------}
getLastVersionForPlatform :: [Char] → IO String
getLastVersionForPlatform p = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" ++ p ++ "/LAST_CHANGE"
        in simpleHttp url
            >>= \bs → return $ S.decode $ L.unpack bs
{-------------------------  Chromium  --------------------------------------}
getChromium :: [Char] → [Char] → IO()
getChromium p v = withSocketsDo $ do
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                        ++ p ++ "/" ++ v ++ "/mini_installer.exe"
    irequest <- liftIO $ parseUrl url
    withManager $ \manager → do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile "mini-installer.exe"
{----------------------------------------------------------------------------------------}