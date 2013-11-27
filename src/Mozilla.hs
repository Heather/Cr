module Mozilla
  (  getUX
  ) where

import System.IO
import System.Exit
import System.Directory
import System.Process

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit

import Data.List
import Data.Conduit.Binary (sinkFile)

import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad
import Control.Monad.IO.Class (liftIO)
{-------------------------  Chromium  --------------------------------------}
getUX :: [Char] -> IO()
getUX fname = withSocketsDo $ do
    let url = "http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-ux/" 
              ++ fname
    irequest <- liftIO $ parseUrl url
    fileExist <- doesFileExist fname
    when fileExist $ do
        putStrLn " -> Removing old version"
        removeFile fname
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile fname
{----------------------------------------------------------------------------------------}
