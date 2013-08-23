{-# LANGUAGE UnicodeSyntax #-}

module Gclient
  ( getGitSources
  ) where

import Depot

import System.IO
import System.Directory
{-------------------------------  Depot Tools  -----------------------------------------}
getGitSources :: [Char] → IO()
getGitSources p =
    case p of
     "Win" -> do
        doesFileExist "depot_tools.zip" >>= \fileExist → if not fileExist 
            then print " -> Getting Depot Tools" >> getDepotTools p
            else print ""
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------------------------------------------------------------}
