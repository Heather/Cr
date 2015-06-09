{-# LANGUAGE
    UnicodeSyntax
  , Safe
  #-}

module Win
  ( getShellFolder
  , getDesktopFolder
  , getShellRoamingFolder
  ) where

import System.Win32.Types
import System.Win32.Shell

getShellFolder ∷ IO String
getShellFolder = sHGetFolderPath nullPtr cSIDL_LOCAL_APPDATA nullPtr 0

getDesktopFolder ∷ IO String
getDesktopFolder = sHGetFolderPath nullPtr cSIDL_DESKTOPDIRECTORY nullPtr 0

getShellRoamingFolder ∷ IO String
getShellRoamingFolder = sHGetFolderPath nullPtr cSIDL_APPDATA nullPtr 0
