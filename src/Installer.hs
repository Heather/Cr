{-# LANGUAGE CPP, MultiWayIf, LambdaCase #-}

module Installer
  ( showV
  , showHelp
  , showChromeVersion
  , fireFox, ya, dartIum
  , fireFoxR, yandexR, dartIumR
  , install
  ) where

import Yaml
import CommonDataStorage

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Win
#endif

import Text.Printf
import Text.Show

import System.Console.GetOpt
import System.Directory
import System.Process
import System.Exit
import System.IO
import System.Info (os)
import System.Environment( getEnv )
import System.FilePath(takeDirectory, (</>))
import System.Environment.Executable ( getExecutablePath )

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

import qualified Paths_Cr as My
import Data.Version (showVersion)

showV _    =    printf (showVersion My.version) >> exitWith ExitSuccess
showHelp o _ = do putStrLn $ usageInfo "Usage: Cr [optional things]" o
                  exitWith ExitSuccess

showChromeVersion _ = do getLastVersionForPlatform "Win"
                            >>= printf "last: %s\n"
                         exitWith ExitSuccess

cSwrap = bracket_
     ( do   putStrLn " ________________________________________________________ "
            putStrLn "          And who the hell do you think I've become?      "
            putStrLn "  Like the person inside, I've been opening up.           "
            putStrLn "                            I'm onto you. (I'm onto you.) "
            putStrLn " ________________________________________________________ "
    )( do   putStrLn " ________________________________________________________ "
            putStrLn " Cut out your tongue and feed it to the liars.            "
            putStrLn "     Black hearts shed light on dying words.              "
            putStrLn "                                                          "
            putStrLn "                                 I wanna feel you burn.   "
            putStrLn " ________________________________________________________ "
            putStrLn ""
    )

simpleInstall exeFile fname getF config = do
    let base = basedir config
        ux   = base </> exeFile
    uxExists <- doesFileExist ux
    if | uxExists  -> createProcess (proc ux []) >> return () -- TODO
       | otherwise -> cSwrap $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
            putStrLn $ " -> Warning: " ++ exeFile ++ " will be killed soon"
#endif
            printf " -> Downloading\n" >> getF fname
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
            pidk <- runCommand $ "taskkill /im " ++ exeFile ++ " /f"
            waitForProcess pidk >> return ()
#endif
            putStrLn " -> Installing"
            pid <- runCommand fname
            waitForProcess pid >>= \exit -> do
                fileExist <- doesFileExist fname
                when fileExist $ do
                    putStrLn " -> Clean Up"
                    removeFile fname
                exitWith exit
    exitWith ExitSuccess

fireFox config = simpleInstall "firefox.exe" fname (download Firefox) config
  where fname = "firefox-" ++ (version config) 
                           ++ ".en-US.win32.installer.exe"
ya      = simpleInstall "Yandex.exe" "Yandex.exe" $ download Yandex
dartIum = simpleInstall "chrome.exe" "dartium-windows-ia32-release.zip" $ download Dartium

getConfig :: IO String
getConfig =
    if | os `elem` ["win32", "mingw32", "cygwin32"] -> (</> "Cr.yml") 
                                                        <$> takeDirectory 
                                                        <$> getExecutablePath
       | otherwise -> return "/etc/Cr.yml"

openConfig :: String -> IO Config
openConfig ymlx =
    doesFileExist ymlx >>= \isCfgEx ->
        if isCfgEx then yDecode ymlx :: IO Config
                   else return Config { installed="0"
                                      , mozilla=False
                                      , dartium=False
                                      , yandex=False
                                      , version="33.0a1"
                                      , basedir="C:\\Program Files\\Nightly"
                                      , autoclose=False
                                }

fireFoxR _ = fireFox =<< openConfig =<< getConfig
yandexR _  = ya =<< openConfig =<< getConfig
dartIumR _ = dartIum =<< openConfig =<< getConfig

install :: String -> String -> Bool -> Bool -> IO()
install bl pl force run = do
    when (not run) $ do
        ymlx   <- getConfig
        config <- openConfig ymlx
        putStrLn $ " Cr " ++ (showVersion My.version)
        if | mozilla config -> fireFox config
           | dartium config -> dartIum config
           | yandex config -> ya config
           | otherwise -> cSwrap $ do
            let installedNow = installed config
            ls <- if bl == "last"
                    then do putStrLn " -> Checking for the last version"
                            try $ getLastVersionForPlatform pl
                                :: IO (Either SomeException String)
                            >>= \case Left what -> do putStrLn $ show what
                                                      return installedNow
                                      Right val -> return val
                    else return bl
            if installedNow == ls && not force
                then putStrLn " -> This version is installed"
                else do let acls = autoclose config
                            new_config  = config { installed = ls }
                            fname       = "mini_installer.exe"
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                        when acls $ putStrLn " -> Warning: Chromium will be killed soon"
#endif
                        printf " -> Downloading %s\n" ls
                            >> getChromium pl ls fname

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                        when acls $ do pidk <- runCommand "taskkill /im chrome.exe /f"
                                       waitForProcess pidk >> return ()
#endif
                        putStrLn " -> Installing"
                        pid <- runCommand fname
                        waitForProcess pid >>= \_ -> do
                            putStrLn " -> Clean Up"
                            userProfilePath  <- getEnv "UserProfile"
                            let removeIfExist [x]    = doesFileExist x >>= (flip when $ removeFile x)
                                removeIfExist (x:xs) = do removeIfExist [x]
                                                          removeIfExist xs
                                removeIfExist []     = return ()
                            removeIfExist [fname
                                , userProfilePath </> "Desktop" </> "Chromium.lnk"
                                , userProfilePath </> "AppData"
                                                  </> "Roaming" </> "Microsoft" </> "Internet Explorer"
                                                  </> "Quick Launch" </> "User Pinned" </> "TaskBar" </> "Chromium.lnk"]                            
                            putStrLn " -> Update installed version"
                            yEncode ymlx new_config
    putStrLn " -> Running"

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    getShellFolder >>= \shellfolder ->
        let pchromium = shellfolder </> "Chromium\\Application\\chrome.exe"
        in createProcess (proc pchromium []) >> return ()
#endif
