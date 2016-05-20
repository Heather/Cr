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

import qualified Data.ByteString.Char8 as BS

data Config = Config { works ∷ String
                     , installed ∷ String
                     , autoclose ∷ Bool
                     } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config where
  toJSON = genericToJSON defaultOptions

yDecode :: FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode μ = do
  ymlData ← BS.readFile μ
  return $ case decodeEither ymlData of
                  Left er → error er
                  Right r → r

yEncode :: ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode μ δ = BS.writeFile μ $ encode δ
