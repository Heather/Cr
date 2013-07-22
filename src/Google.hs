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

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C

import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Network.HTTP.Types
import Control.Monad.IO.Class (liftIO)

{------------------------- Last Chromium Version --------------------------------------}
getLastVersionForPlatform :: [Char] → IO String
getLastVersionForPlatform s = withSocketsDo
    $   let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" ++ s ++ "/LAST_CHANGE"
        in simpleHttp url
            >>= \bs → return $ S.decode $ L.unpack bs
{-------------------------  Chromium  --------------------------------------}
getChromium :: [Char] → [Char] → IO()
getChromium s v = withSocketsDo $ do
    -- with https : 
    --
    --     certificate verify chain doesn't yet work on your platform
    --
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                        ++ s ++ "/" ++ v ++ "/mini_installer.exe"
    irequest <- liftIO $ parseUrl url
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile "mini-installer.exe"
{----------------------------------------------------------------------------------------}