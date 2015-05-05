{-# LANGUAGE CPP, MultiWayIf, LambdaCase, UnicodeSyntax #-}

module Installer
  ( showV
  , showHelp
  , showChromeVersion
  , fireFoxR, yandexR, dartIumR
  , install
  ) where

import Yaml
import Misc
import Downloader
import InstallHelper
import CustomInstaller

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Win
#endif

import Text.Printf
import Text.Show

import System.Directory
import System.Process
import System.Exit
import System.IO
import System.Environment (getEnv)
import System.FilePath ((</>))

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

import Prelude.Unicode

showChromeVersion _ = do getLastVersionForPlatform "Win_x64" -- Win
                            >>= printf "last: %s\n"
                         exitWith ExitSuccess

install ∷ String → String → Bool → Bool → IO()
install bl pl force run = do
    when (not run) $ do
        ymlx   ← getConfig
        config ← openConfig ymlx
        putStrLn $ " Cr " ++ showMyV
        if | mozilla config → fireFox config
           | dartium config → dartIum config
           | yandex config → ya config
           | otherwise → cSwrap $ do
            let installedNow = installed config
            ls ← if bl == "last"
                    then do putStrLn " -> Checking for the last version"
                            try $ getLastVersionForPlatform pl
                                ∷ IO (Either SomeException String)
                            >>= \case Left what → do putStrLn $ show what
                                                     return installedNow
                                      Right val → return val
                    else return bl
            if installedNow == ls ∧ not force
                then putStrLn " -> This version is installed"
                else do let acls = autoclose config
                            new_config  = config { installed = ls }
                            fname       = "mini_installer.exe"
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                        when acls $ putStrLn " -> Warning: Chromium will be killed soon"
#endif
                        printf " -> Downloading %s\n" ls
                        getChromium pl ls fname `catch` (
                              \err → do putStrLn $ show (err ∷ IOException)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                                        putStrLn "Press any key.."
                                        getChar >> return ()
#endif
                                    )
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                        when acls $ do pidk ← runCommand "taskkill /im chrome.exe /f"
                                       waitForProcess pidk >> return ()
#endif
                        putStrLn " -> Installing"
                        pid ← runCommand fname
                        waitForProcess pid >>= \_ → do
                            putStrLn " -> Clean Up"
                            let removeIfExist [x]    = doesFileExist x >>= (flip when $ removeFile x)
                                removeIfExist (x:xs) = do removeIfExist [x]
                                                          removeIfExist xs
                                removeIfExist []     = return ()
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                            desktop ← getDesktopFolder
                            appshell ← getShellRoamingFolder
                            removeIfExist [fname
                                , desktop  </> "Chromium.lnk"
                                , appshell </> "Microsoft" </> "Internet Explorer"
                                           </> "Quick Launch" </> "User Pinned"
                                           </> "TaskBar" </> "Chromium.lnk"]
#else
                            removeIfExist [fname]
#endif
                            putStrLn " -> Update installed version"
                            yEncode ymlx new_config

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        putStrLn " -> Running"
    getShellFolder >>= \shellfolder →
        let pchromium = shellfolder </> "Chromium\\Application\\chrome.exe"
        in createProcess (proc pchromium []) >> return ()
#endif
