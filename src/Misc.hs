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

showV       :: ∀ τ β. τ → IO β
showV _      = printf showMyV >> exitSuccess

showHelp    :: ∀ τ β α. [OptDescr α] → τ → IO β
showHelp o _ = putStrLn (usageInfo "Usage: Cr [optional things]" o)
                  >> exitSuccess
