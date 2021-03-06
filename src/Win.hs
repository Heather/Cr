{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module Win
  ( getShellFolder
  , getDesktopFolder
  , getShellRoamingFolder
  ) where

import           System.Win32.Types
import           System.Win32.Shell


#if MIN_VERSION_Win32(2,3,1)
getShellFolder ∷ IO String
getShellFolder = sHGetFolderPath nullPtr cSIDL_LOCAL_APPDATA nullPtr 0

getDesktopFolder ∷ IO String
getDesktopFolder = sHGetFolderPath nullPtr cSIDL_DESKTOPDIRECTORY nullPtr 0
#else
import Graphics.Win32.GDI.Types
import Foreign.C.String
import Foreign.Marshal.Array

-- I'm really not sure now whether it should use stdcall or ccall
-- but at least it works with ccall and appveyour wants it
foreign import ccall unsafe "SHGetFolderPathW"
    cSHGetFolderPathW ∷ HWND → INT → HANDLE → DWORD → CWString → IO LONG

maxPath = 260
cSIDL_LOCAL_APPDATA = 0x001c -- ShlObj.h in MS Platform SDK
cSIDL_DESKTOPDIRECTORY = 0x0010

getShellFolder ∷ IO String
getShellFolder = allocaArray0 maxPath $ \path → do
    cSHGetFolderPathW nullHANDLE cSIDL_LOCAL_APPDATA nullHANDLE 0 path
    peekCWString path

getDesktopFolder ∷ IO String
getDesktopFolder = allocaArray0 maxPath $ \path → do
    cSHGetFolderPathW nullHANDLE cSIDL_DESKTOPDIRECTORY nullHANDLE 0 path
    peekCWString path
#endif

getShellRoamingFolder ∷ IO String
getShellRoamingFolder = sHGetFolderPath nullPtr cSIDL_APPDATA nullPtr 0
