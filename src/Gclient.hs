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
            {-          Here depot_tools must be added to PATH             -}
            putStrLn "======================================================"
            putStrLn " -> NOW! Move your ass and add depot_tools to PATH" 
            putStrLn " -> Press any key when it will be done or already done"
            putStrLn "======================================================"
            getChar >> return ()
            {- I know..................................................... -}
            pid <- runCommand $ ddir ++ "\\gclient"
            putStrLn $ ddir ++ "/gclient"
            waitForProcess pid >>= \exitWith → 
                putStrLn ""
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------  gclient  -------------------------------------------}
gClient :: [Char] → IO()
gClient args = do
    let ddir = "depot_tools"
    {- TODO: Handle POSIX -}
    pid <- runCommand $ ddir ++ "\\gclient " ++ args
    waitForProcess pid >>= \exitWith → putStrLn ""
{----------------------------------------------------------------------------------------}
