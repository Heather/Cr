{-# LANGUAGE UnicodeSyntax, CPP, MultiWayIf #-}

import CommonDataStorage

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Win
#endif

import Text.Printf
import System.Environment( getArgs )
import System.Directory
import System.Process
import System.Exit
import System.Console.GetOpt
import System.Info (os)
import System.IO (withFile, Handle, IOMode(WriteMode), hPutStr)

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

version = "0.2.4"
main = do
    user <- getAppUserDataDirectory "Cr.lock"
    locked <- doesFileExist user
    if locked then putStrLn "There is already one instance of this program running."
              else myThreadId >>= \t → withFile user WriteMode (do_program t)
                                       `finally` removeFile user

data Options = Options  {
    optPlatform  :: String,
    optBuild :: String → IO()
  }
defaultOptions :: Options
defaultOptions = Options {
    optPlatform = if | os `elem` ["win32", "mingw32", "cygwin32"] → "Win"
                     | os `elem` ["darwin"] → "Mac"
                     | otherwise → "Linux"
        ,
    optBuild = go "last"
  }
do_program :: ThreadId → Handle → IO ()
do_program t h = let s = "Locked by thread: " ++ show t
                 in do  putStrLn s
                        hPutStr h s
                        args <- getArgs
                        let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) actions
                        let Options { optPlatform = platform,
                                    optBuild = build } = opts
                        build platform

options :: [OptDescr (Options → IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Cr version number",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['d'] ["dartium"] (NoArg getDartium) "Get dartium",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number"
  ]

showVersion _ = printf "\n  Cr v.%s\n\n" version
                    >> exitWith ExitSuccess
    
showHelp _ = do putStrLn $ usageInfo "Usage: Cr [optional things]" options
                exitWith ExitSuccess
  
showChromeVersion _ = do
    ls <- getLastVersionForPlatform "Win"
    printf "last: %s\n" ls
        >> exitWith ExitSuccess

getDartium _ = getDart "Win" >> exitWith ExitSuccess

getp arg opt = return opt { optPlatform = arg }
getb arg opt = return opt { optBuild = go arg }

data Config = Config { cr :: String
                     , installed  :: Int
    } deriving (Read, Show)
readConfig :: String → Config
readConfig = read
writeConfig :: Config → String
writeConfig = show

go :: String → String → IO()
go bl pl = do
    let cfg = "Cr.cfg"
    config <- doesFileExist cfg >>= \isCfgEx →
                if isCfgEx then readFile cfg >>= return . readConfig
                           else return Config{cr="Win", installed=0}
    case (cr config) of
     "Dart"                 → getDartium ""
     "JustShowVersion"      → showChromeVersion ""
     _                      → do                {- default // Installation // -}
        printf "\n  Cr v.%s\n\n" version                {-  Intro  -}
        putStrLn " ________________________________________________________ "
        ls <- if bl == "last"
                then do putStrLn " -> Checking for the last version"
                        getLastVersionForPlatform pl
                else (return bl)
        let ils = read ls :: Int
        if (installed config) >= ils 
            then putStrLn " -> Installed version is newer or the same"
            else do let new_config = config{installed=ils}
                    let fname = "mini_installer.exe"
                    printf " -> Getting %s\n" ls
                        >> getChromium pl ls fname
                    
                    putStrLn " -> Installing"
                    pid <- runCommand fname
                    waitForProcess pid >>= \exitWith → do
                        fileExist <- doesFileExist fname
                        when fileExist $ do
                            putStrLn " -> Clean Up"
                            removeFile fname
                        writeFile cfg $ writeConfig new_config
        putStrLn " -> Done"

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        putStrLn " ________________________________________________________ "
        putStrLn " -> Running"
        getShellFolder >>= \shellfolder →
            let chromium = shellfolder ++ "\\Chromium\\Application\\chrome.exe"
            in createProcess (proc chromium [])
#endif

        putStrLn " ________________________________________________________ "
        putStrLn ""
