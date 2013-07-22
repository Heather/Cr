{-# LANGUAGE UnicodeSyntax #-}
module Google
  ( getLastVersionForPlatform
  , getChromium
  ) where

import Data.List
import Data.Function
import Data.Char
import Data.IORef
import Data.Maybe

import Control.Monad
import Control.Applicative

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S
{------------------------- Last Chromium Version --------------------------------------}
getLastVersionForPlatform :: [Char] → IO String
getLastVersionForPlatform s = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" ++ s ++ "/LAST_CHANGE"
        in simpleHttp url
            >>= \bs → return $ S.decode $ L.unpack bs
{-------------------------  Chromium  --------------------------------------}
getChromium :: [Char] → [Char] → IO()
getChromium s v = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/index.html?path=" 
                    ++ s ++ "/" ++ v ++ "/mini-installer.exe"
        in simpleHttp url >>= L.writeFile "mini-installer.exe"
{----------------------------------------------------------------------------------------}