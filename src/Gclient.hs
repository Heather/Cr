{-# LANGUAGE UnicodeSyntax #-}

module Gclient
  ( gInit,
    gClient
  ) where

import Depot

import Codec.Archive.Zip

import System.IO
import System.Directory
import System.Process

import Control.Monad

import qualified Data.ByteString.Lazy as B
{-------------------------------  GettingGclientReady  ----------------------------------}
gInit :: [Char] → IO()
gInit p =
    case p of
     "Win" -> do
        let ddir = "depot_tools"
        doesDirectoryExist "depot_tools" >>= \dirExist → unless dirExist $ do
            let tarball = "depot_tools.zip"
            doesFileExist tarball >>= \fileExist → unless fileExist $ do
                putStrLn " -> Getting Depot Tools" 
                getDepotTools p
                dictZipFile <- B.readFile tarball
                extractFilesFromArchive [] $ toArchive dictZipFile
            {- Here depot_tools must be added to PATH -}
            pid <- runCommand $ ddir ++ "gclient"
            waitForProcess pid >>= \exitWith → putStrLn " -> Done"
     _  -> putStrLn "This platform is not supported yet :("
{-------------------------------  Depot Tools  -----------------------------------------}
gClient :: [Char] → IO()
gClient args = do
    let ddir = "depot_tools"
    pid <- runCommand $ ddir ++ "gclient" ++ args
    waitForProcess pid >>= \exitWith → putStrLn ""
{----------------------------------------------------------------------------------------}
