{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  , DeriveGeneric
  #-}

module Yaml
  ( Config(..)
  , FromJSON
  , ToJSON
  , yDecode
  , yEncode
  ) where

import GHC.Generics

import Data.Yaml

-- TODO: ghc bug
-- https://ghc.haskell.org/trac/ghc/ticket/10959
import Data.Aeson.Types (genericToJSON, defaultOptions)

import Control.Applicative
import Control.Applicative.Unicode

import qualified Data.ByteString.Char8 as BS

data Config = Config { works ∷ String
                     , installed ∷ String
                     , autoclose ∷ Bool
                     } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config where
  toJSON = genericToJSON defaultOptions

yDecode :: FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode fnm = do
  ymlData ← BS.readFile fnm
  return $ case decodeEither ymlData of
                  Left er → error er
                  Right r → r

yEncode :: ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ encode dat
