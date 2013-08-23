{-# LANGUAGE UnicodeSyntax #-}

module Depot
  ( getDepotTools
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
{-------------------------------  Depot Tools  -----------------------------------------}
getDepotTools :: [Char] → IO()
getDepotTools p = 
    case p of
     "Win" -> withSocketsDo $ do
        let url = "http://src.chromium.org/svn/trunk/tools/depot_tools.zip"
        irequest <- liftIO $ parseUrl url
        withManager $ \manager → do
            let request = irequest
                 { method = methodGet }
            response <- http request manager
            responseBody response C.$$+- sinkFile "depot_tools.zip"
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------------------------------------------------------------}
