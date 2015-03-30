{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Yaml
  ( Config(..)
  , FromJSON
  , ToJSON
  , yDecode
  , yEncode
  ) where

import Data.Yaml

import Control.Applicative
import Control.Applicative.Unicode

import qualified Data.ByteString.Char8 as BS

data Config = Config { installed ∷ String
                     , mozilla   ∷ Bool
                     , dartium   ∷ Bool
                     , yandex    ∷ Bool
                     , version   ∷ String
                     , basedir   ∷ String
                     , autoclose ∷ Bool
                     } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "installed" ⊛
                           v .: "mozilla" ⊛
                           v .: "dartium" ⊛
                           v .: "yandex" ⊛
                           v .: "version" ⊛
                           v .: "basedir" ⊛
                           v .: "autoclose"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Config from YAML/JSON"

instance ToJSON Config where
   toJSON (Config inst moz dar yan ver base
                    acls) = object [ "installed"  .= inst
                                   , "mozilla"    .= moz
                                   , "dartium"    .= dar
                                   , "yandex"     .= yan
                                   , "version"    .= ver
                                   , "basedir"    .= base
                                   , "autoclose"  .= acls
                                   ]

yDecode ∷ FromJSON iFromJSONable => FilePath → IO iFromJSONable
yDecode fnm = do
    ymlData ← BS.readFile fnm
    return $ case Data.Yaml.decode ymlData of
                Just decoded → decoded
                Nothing      → error "Can't parse from YAML"

yEncode ∷ ToJSON iToJSONable => FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ Data.Yaml.encode dat
