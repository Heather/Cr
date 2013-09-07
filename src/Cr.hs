{-# LANGUAGE UnicodeSyntax, CPP, MultiWayIf #-}

import CommonDataStorage
import Gclient

import Text.Printf
import System.Environment( getArgs )
import System.Directory
import System.Process
import System.Exit
import System.Console.GetOpt
import System.Info (os)

import Control.Monad
import Control.Applicative

version = "0.1.7"
main = do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optPlatform = platform,
                optBuild = build } = opts
    build platform

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

options :: [OptDescr (Options → IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Cr version number",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['s'] ["src"]     (NoArg getSrc) "Get chromium sources",
    Option ['d'] ["dartium"] (NoArg getDartium) "Get dartium",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number"
  ]

showVersion _ =
    printf "\n  Cr v.%s\n\n" version
        >> exitWith ExitSuccess
    
showHelp _ = do
    putStrLn $ usageInfo "Usage: Cr [optional things]" options
    exitWith ExitSuccess
  
showChromeVersion _ = do
    ls <- getLastVersionForPlatform "Win"
    printf "last: %s\n" ls
        >> exitWith ExitSuccess

getSrc _ =
    gInit "Win"
        >> fetch "chromium"
        >> exitWith ExitSuccess
        
getDartium _ =
    getDart "Win"
        >> exitWith ExitSuccess

getp arg opt = return opt { optPlatform = arg }
getb arg opt = return opt { optBuild = go arg }

data Config = Config
  { cr :: String
  , installed  :: Int
  } deriving (Read, Show)
readConfig :: String → Config
readConfig = read
writeConfig :: Config → String
writeConfig = show

go :: String → String → IO()
go bl pl = do
    config <- readFile "Cr.cfg" >>= return . readConfig
    case (cr config) of
     "Src" → getSrc ""
     "Dart" → getDartium ""
     "JustShowVersion" → showChromeVersion ""
     _ → do {- default // Installation // -}
        printf "\n  Cr v.%s\n\n" version  {-  Intro  -}
        putStrLn " ========================== " 
        ls <- if bl == "last"
                then do putStrLn " -> Checking for the last version"
                        getLastVersionForPlatform pl
                else (return bl)
        let ils = read ls :: Int
        if (installed config) >= ils 
            then putStrLn " -> Installed version is newer or the same"
            else do
                let new_config = config{installed=ils}
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
                    writeFile "Cr.cfg" $ writeConfig new_config
                    putStrLn " -> Done" -- AppData\Local\Chromium\Application
                    putStrLn " ========================== "
                    putStrLn ""
