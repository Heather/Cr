{-# LANGUAGE UnicodeSyntax #-}

module Gclient
  ( getGitSources
  ) where

import Depot

import Codec.Archive.Zip

import System.IO
import System.Directory

import Control.Monad

import qualified Data.ByteString.Lazy as B
{-------------------------------  Depot Tools  -----------------------------------------}
getGitSources :: [Char] → IO()
getGitSources p = do
    case p of
     "Win" -> do
        doesDirectoryExist "depot_tools" >>= \dirExist → unless dirExist $ do
            let tarball = "depot_tools.zip"
            doesFileExist tarball >>= \fileExist → unless fileExist $ do
                print " -> Getting Depot Tools" 
                    >> getDepotTools p
                dictZipFile <- B.readFile tarball
                extractFilesFromArchive [] $ toArchive dictZipFile
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------------------------------------------------------------}
