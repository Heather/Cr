{-# LANGUAGE UnicodeSyntax, CPP, MultiWayIf #-}

import Text.Printf
import System.Environment( getArgs )
import System.Process
import System.Exit
import System.Console.GetOpt
import System.Info (os)

import Google

import Data.Maybe( fromMaybe )

version = "0.0.2"
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
                     | otherwise -> "Linux"
        ,
    optBuild = go "last"
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Cr version number",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number"
  ]

showVersion _ = do
  printf "\n  Cr v.%s\n\n" version
  exitWith ExitSuccess
  
showChromeVersion _ = do
  ls <- getLastVersionForPlatform "Win"
  printf "last: %s\n" ls
  exitWith ExitSuccess
  
getp arg opt = return opt { optPlatform = arg }
getb arg opt = return opt { optBuild = go arg }

go :: String → String → IO()
go bl pl = do
    printf "\n  Cr v.%s\n\n" version  {-  Intro  -}
    
    printf "\n ========================== " 
    ls <- if bl == "last"
            then do 
                printf "\n -> Checking for the last version"
                getLastVersionForPlatform pl
            else (return bl)
    
    printf "\n -> Getting %s" ls
    getChromium "Win" ls
    
    printf "\n -> Installing"
    pid <- runCommand "mini-installer.exe"
    waitForProcess pid >>= \exitWith → do
        printf "\n -> Done"
        printf "\n ========================== "
        printf "\n"

        -- > Wait for keypress (Only for windows)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        getChar >> return () -- return nothing but IO
#endif
