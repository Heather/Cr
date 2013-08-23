{-# LANGUAGE UnicodeSyntax #-}

module Gclient
  ( getGitSources
  ) where

import Depot

import System.IO
import System.Directory

import Control.Monad
{-------------------------------  Depot Tools  -----------------------------------------}
getGitSources :: [Char] → IO()
getGitSources p = do
    case p of
     "Win" -> do
        doesFileExist "depot_tools.zip" >>= \fileExist → unless fileExist
            $ print " -> Getting Depot Tools" >> getDepotTools p
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------------------------------------------------------------}
