{-# LANGUAGE ForeignFunctionInterface #-}

module Win
  ( getShellFolder
  ) where

import System.Win32.Types
import Graphics.Win32.GDI.Types
import Foreign.C.String
import Foreign.Marshal.Array

foreign import stdcall unsafe "SHGetFolderPathW" --ccall
    cSHGetFolderPathW :: HWND -> INT -> HANDLE -> DWORD -> CWString -> IO LONG

maxPath = 260
cSIDL_LOCAL_APPDATA = 0x001c -- ShlObj.h in MS Platform SDK

getShellFolder :: IO String
getShellFolder = allocaArray0 maxPath $ \path -> do
    cSHGetFolderPathW nullHANDLE cSIDL_LOCAL_APPDATA nullHANDLE 0 path
    peekCWString path
