{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}

module Yaml
  ( Config(..)
  , FromJSON
  , ToJSON
  , yDecode
  , yEncode
  ) where

import Data.Yaml
import Data.Maybe (fromMaybe)

import Control.Applicative
import Control.Applicative.Unicode

import qualified Data.ByteString.Char8 as BS

data Config = Config { works ∷ String
                     , installed ∷ String
                     , autoclose ∷ Bool
                     } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                            v .: "works" ⊛
                            v .: "installed" ⊛
                            v .: "autoclose"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Config from YAML/JSON"

instance ToJSON Config where
   toJSON (Config works inst acls) = object
                            [ "works" .= works
                            , "installed"  .= inst
                            , "autoclose"  .= acls
                            ]

yDecode ∷ FromJSON iFromJSONable => FilePath → IO iFromJSONable
yDecode fnm = do
    ymlData ← BS.readFile fnm
    return $ fromMaybe (error "Can't parse from YAML")
                       (decode ymlData)

yEncode ∷ ToJSON iToJSONable => FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ Data.Yaml.encode dat
