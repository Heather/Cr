{-# LANGUAGE UnicodeSyntax, CPP #-}

import Text.Printf
import System.Environment( getArgs )
import System.Process
import System.Exit
import System.Console.GetOpt

import Google

import Data.Maybe( fromMaybe )

version = "0.0.1"
main = do
  args <- getArgs
  let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optPlatform = platform,
                optBuild = build } = opts
  platform >>= build

data Options = Options  {
    optPlatform  :: IO String,
    optBuild :: String → IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    --TODO: Detect platform
    optPlatform  = (return "Win"),
    optBuild = go "last"
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['V'] ["version"] (NoArg showVersion) "show version number",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number"
  ]

showVersion _ = do
  printf "\n  Cr v.%s\n\n" version
  exitWith ExitSuccess
  
getp arg opt = return opt { optPlatform = return arg }
getb arg opt = return opt { optBuild = go arg }

go :: String → String → IO()
go bl pl = do
    printf "\n  Cr v.%s\n\n" version  {-  Intro  -}
    
    printf "\n ========== " 
    ls <- if bl == "last"
            then do 
                printf "\n -> Checking flor last version"
                getLastVersionForPlatform pl
            else (return bl)
    
    printf "\n -> Getting %s" ls
    getChromium "Win" ls
    
    printf "\n -> Installing"
    pid <- runCommand "mini-installer.exe"
    waitForProcess pid >>= \exitWith → do
        printf "\n -> Done"
        printf "\n ========== "
        printf "\n"

        -- > Wait for keypress (Only for windows)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        getChar >> return () -- return nothing but IO
#endif