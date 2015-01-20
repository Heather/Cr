module Misc
  ( showMyV
  , showV
  , showHelp
  ) where

import Text.Printf
import Text.Show

import System.Exit
import System.Console.GetOpt

import qualified Paths_Cr as My
import Data.Version (showVersion)

showMyV      = showVersion My.version
showV _      = printf showMyV >> exitWith ExitSuccess
showHelp o _ = do putStrLn $ usageInfo "Usage: Cr [optional things]" o
                  exitWith ExitSuccess
