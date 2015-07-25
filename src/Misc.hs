{-# LANGUAGE
    UnicodeSyntax
  , RankNTypes
  #-}

module Misc
  ( showMyV
  , showV
  , showHelp
  ) where

import Text.Printf

import System.Exit
import System.Console.GetOpt

import qualified Paths_Cr as My
import Data.Version (showVersion)

showMyV     :: String
showMyV      = showVersion My.version

showV       :: ∀ t b. t → IO b
showV _      = printf showMyV >> exitSuccess

showHelp    :: ∀ t b a. [OptDescr a] → t → IO b
showHelp o _ = putStrLn (usageInfo "Usage: Cr [optional things]" o)
                  >> exitSuccess
