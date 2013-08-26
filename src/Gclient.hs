{-# LANGUAGE UnicodeSyntax #-}

module Gclient
  ( gInit,
    gClient,
    fetch
  ) where

import Depot

import Codec.Archive.Zip

import System.IO
import System.Exit
import System.Directory
import System.Process

import System.FilePath((</>))

import Control.Monad

import qualified Data.ByteString.Lazy as B
{------------------------------------  CopyDir  -----------------------------------------}
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectory dst
    content <- getDirectoryContents src
    let xs = filter (`notElem` [".", ".."]) content
    forM_ xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDir srcPath dstPath
            else copyFile srcPath dstPath
{-------------------------------  GettingGclientReady  ----------------------------------}
gInit :: [Char] → IO()
gInit p =
    case p of
     "Win" -> do
        let src = "depot_tools"
        let dst = "C:/depot_tools"
        doesDirectoryExist src >>= \dirExist → unless dirExist $ do
            let tarball = "depot_tools.zip"
            doesFileExist tarball >>= \fileExist → unless fileExist $ do
                putStrLn " -> Getting Depot Tools" 
                getDepotTools p
                dictZipFile <- B.readFile tarball
                extractFilesFromArchive [OptRecursive, OptVerbose] $ toArchive dictZipFile
                srcExists <- doesDirectoryExist src
                dstExists <- doesDirectoryExist dst
                if or [not srcExists, dstExists] 
                    then putStrLn " -> Can not copy to C:"
                    else do
                        copyDir src dst
                            >> removeDirectoryRecursive src
            {-          Here depot_tools must be added to PATH             -}
            putStrLn "======================================================"
            putStrLn " -> NOW! Move your ass and add C:/depot_tools to PATH" 
            putStrLn " -> Press any key when it will be done or already done"
            putStrLn "======================================================"
            getChar >> return ()
            {- I know..................................................... -}
            pid <- runCommand $ dst </> "gclient"
            waitForProcess pid >>= \exitWith → 
                putStrLn ""
     _  -> putStrLn "This platform is not supported yet :("
{----------------------------------  gclient  -------------------------------------------}
gClient :: [Char] → IO()
gClient args = do
    pid <- runCommand $ "C:/depot_tools/gclient " ++ args
    waitForProcess pid >>= \exitWith → putStrLn ""
{----------------------------------------------------------------------------------------}
fetch :: [Char] → IO()
fetch project = do
    doesDirectoryExist project >>= \dirExist → unless dirExist $ do
        createDirectory project
    pid <- runCommand $ "cd " ++ project ++ "& C:/depot_tools/fetch " ++ project ++ " --nosvn=True"
    waitForProcess pid >>= \exitWith → putStrLn ""
{----------------------------------------------------------------------------------------}
