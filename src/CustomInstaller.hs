{-# LANGUAGE CPP, MultiWayIf, LambdaCase, UnicodeSyntax #-}

module CustomInstaller
  ( fireFox, ya, dartIum
  , fireFoxR, yandexR, dartIumR
  ) where

import Yaml
import InstallHelper
import CustomDownloader

import Text.Printf
import Text.Show

import System.Directory
import System.Process
import System.Exit
import System.IO
import System.FilePath ((</>))

import Control.Concurrent
import Control.Monad
import Control.Applicative

import Control.Monad.Unicode

simpleInstall exeFile fname getF config = do
    let base = basedir config
        ux   = base </> exeFile
    uxExists <- doesFileExist ux
    if | uxExists  → createProcess (proc ux []) ≫ return () -- TODO
       | otherwise → cSwrap $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
            putStrLn $ " -> Warning: " ++ exeFile ++ " will be killed soon"
#endif
            printf " -> Downloading\n" ≫ getF fname
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
            pidk <- runCommand $ "taskkill /im " ++ exeFile ++ " /f"
            waitForProcess pidk ≫ return ()
#endif
            putStrLn " -> Installing"
            pid <- runCommand fname
            waitForProcess pid ≫= \exit → do
                fileExist <- doesFileExist fname
                when fileExist $ do
                    putStrLn " → Clean Up"
                    removeFile fname
                exitWith exit
    exitWith ExitSuccess

fireFox config = simpleInstall "firefox.exe" fname (download Firefox) config
  where fname = "firefox-" ++ (version config)
                           ++ ".en-US.win32.installer.exe"
ya      = simpleInstall "Yandex.exe" "Yandex.exe" $ download Yandex
dartIum = simpleInstall "chrome.exe" "dartium-windows-ia32-release.zip" $ download Dartium

fireFoxR _ = fireFox =≪ openConfig =≪ getConfig
yandexR _  = ya =≪ openConfig =≪ getConfig
dartIumR _ = dartIum =≪ openConfig =≪ getConfig
