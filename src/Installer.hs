{-# LANGUAGE
    CPP
  , MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  #-}

module Installer
  ( showV
  , showHelp
  , showChromeVersion
  , storeWorks
  , install
  ) where

import Yaml
import Misc
import Downloader
import InstallHelper

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Win
#endif

import Text.Printf

import System.Directory
import System.Process
import System.Exit
import System.FilePath ((</>))

import Control.Monad
import Control.Exception

import Prelude.Unicode

showChromeVersion :: ∀ τ β. τ → IO β
showChromeVersion _ = do getLastVersionForPlatform "Win_x64" -- Win
                            >>= printf "last: %s\n"
                         exitSuccess

storeWorks :: ∀ τ β. τ → IO β
storeWorks _ = do
    ψ ← getConfig
    γ ← openConfig ψ
    let υ = installed γ
        ω = γ { works = υ }
    yEncode ψ ω
    putStrLn $ "version " ++ υ ++ " stored for restore"
    exitSuccess

install ∷ String -- build version
        → String -- platform
        → Bool   -- force reinstall
        → Bool   -- run after build
        → Bool   -- restore previous
        → IO()
install β τ force run restore =
    unless run $ cSwrap $ do
        ymlx   ← getConfig
        config ← openConfig ymlx
        let installedNow = installed config
        ls ← if restore
              then return $ works config
              else if β == "last"
                then do putStrLn " -> Checking for the last version"
                        try $ getLastVersionForPlatform τ
                            ∷ IO (Either SomeException String)
                        >>= \case Left what → do print what
                                                 return installedNow
                                  Right val → return val
                else return β

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        present <- -- ensure that chromium is installed
          getShellFolder
          >>= \shellfolder → doesFileExist
                              ( shellfolder
                              </> "Chromium\\Application\\chrome.exe" )
        if installedNow == ls ∧ not force ∧ not restore ∧ present
#else
        if installedNow == ls ∧ not force ∧ not restore
#endif
            then putStrLn " -> This version is installed"
            else do let acls        = autoclose config
                        new_config  = config { installed = ls }
                        fname       = "mini_installer.exe"
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                    when acls $ putStrLn " -> Warning: Chromium will be killed soon"
#endif
                    printf " -> Downloading %s\n" ls
                    getChromium τ ls fname `catch` (
                          \err → print (err ∷ IOException)
                      )
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                    when acls $ do pidk ← runCommand "taskkill /im chrome.exe /f"
                                   waitForProcess pidk >> return ()
#endif
                    putStrLn " -> Installing"
                    pid ← runCommand fname
                    waitForProcess pid >>= \_ → do
                        putStrLn " -> Clean Up"
                        let removeIfExist [x]    = doesFileExist x >>= flip when (removeFile x)
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
    >> getShellFolder >>= \shellfolder →
        let pchromium = shellfolder </> "Chromium\\Application\\chrome.exe"
        in createProcess (proc pchromium []) >> return ()
#endif
