{-# LANGUAGE UnicodeSyntax, MultiWayIf #-}

module Gclient
  ( gInit,
    gClient,
    fetch
  ) where

import Base
import Depot

import Codec.Archive.Zip

import System.IO
import System.Exit
import System.Directory
import System.Process

import System.Info (os)
import System.FilePath((</>))

import Control.Monad

import qualified Data.ByteString.Lazy as B
{-------------------------------  GettingGclientReady  ----------------------------------}
gInit :: [Char] → IO()
gInit p = case p of
    "Win" → let src = "depot_tools"
                dst = "C:/depot_tools"
        in doesDirectoryExist src >>= \dirExist → unless dirExist $ do
            let tarball = "depot_tools.zip"
            doesFileExist tarball >>= \fileExist → unless fileExist $ do
                putStrLn " -> Getting Depot Tools" 
                getDepotTools p
                dictZipFile <- B.readFile tarball
                extractFilesFromArchive [OptVerbose] $ toArchive dictZipFile
                srcExists <- doesDirectoryExist src
                dstExists <- doesDirectoryExist dst
                if or [not srcExists, dstExists] 
                    then putStrLn " -> Can not copy to C:"
                    else copyDir src dst >> removeDirectoryRecursive src
                                         >> removeFile tarball
            {-          Here depot_tools must be added to PATH             -}
            putStrLn "======================================================"
            putStrLn " -> NOW! Move your ass and add C:/depot_tools to PATH" 
            putStrLn " -> Press any key when it will be done or already done"
            putStrLn "======================================================"
            getChar >> return ()
            {- I know..................................................... -}
            pid <- runCommand $ dst </> "gclient"
            waitForProcess pid >>= \exitWith → return ()
    _  → putStrLn "This platform is not supported yet :("
{----------------------------------  gclient  -------------------------------------------}
gClient :: [Char] → IO()
gClient args = do
    pid <- runCommand $ "C:/depot_tools/gclient " ++ args
    waitForProcess pid >>= \exitWith → putStrLn ""
{----------------------------------------------------------------------------------------}
fetch :: [Char] → IO()
fetch project =
    let pDir = if | os `elem` ["win32", "mingw32", "cygwin32"] → "C:/" </> project
                  | otherwise → project
    in doesDirectoryExist pDir >>= \dirExist → 
        if dirExist
            then do createDirectory pDir
                    pid <- runCommand $ "cd " ++ pDir ++ "& C:/depot_tools/fetch " ++ project ++ " --nosvn=True"
                    waitForProcess pid >>= \exitWith → putStrLn " -> Fetch complete"
            else do pid <- runCommand $ "cd " ++ pDir ++ "& C:/depot_tools/gclient update"
                    waitForProcess pid >>= \exitWith → putStrLn " -> Update complete"
{----------------------------------------------------------------------------------------}
