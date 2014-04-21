{-# LANGUAGE OverloadedStrings #-}

module Yaml
  ( Config(..),
    FromJSON,
    ToJSON,
    yDecode,
    yEncode
  ) where

import Data.Yaml
import Data.Maybe (fromJust)

import Control.Applicative

import qualified Data.ByteString.Char8 as BS

data Config = Config {installed :: Int,
                      mozilla :: Bool,
                      version :: String,
                      basedir :: String}
                     deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "installed" <*>
                           v .: "mozilla" <*>
                           v .: "version" <*>
                           v .: "basedir"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Config from YAML/JSON"

instance ToJSON Config where
   toJSON (Config inst moz ver base) = object [ "installed"  .= inst
                                              , "mozilla"    .= moz
                                              , "version"    .= ver
                                              , "basedir"    .= base]

yDecode :: FromJSON iFromJSONable => FilePath -> IO iFromJSONable
yDecode fnm = do
    ymlData <- BS.readFile fnm
    return $ fromJust $ Data.Yaml.decode ymlData

yEncode :: ToJSON iToJSONable => FilePath -> iToJSONable -> IO()
yEncode fnm dat = do
  let bs = Data.Yaml.encode dat
  BS.writeFile fnm bs
