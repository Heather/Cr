{-# LANGUAGE UnicodeSyntax #-}

module Base
  ( copyDir
  ) where

import System.IO
import System.Exit
import System.Directory

import Control.Monad

import System.FilePath((</>))
{------------------------------------  CopyDir  -----------------------------------------}
copyDir ::  FilePath → FilePath → IO ()
copyDir src dst = do
    createDirectory dst
    content <- getDirectoryContents src
    let xs = filter (`notElem` [".", ".."]) content
    forM_ xs $ \name → let srcPath = src </> name
                           dstPath = dst </> name
        in doesDirectoryExist srcPath >>= \dirExist →
            if dirExist then copyDir srcPath dstPath
                        else copyFile srcPath dstPath
{----------------------------------------------------------------------------------------}
