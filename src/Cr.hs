{-# LANGUAGE CPP, MultiWayIf #-}

import CommonDataStorage

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Win
#endif

import Text.Printf
import Text.Show

import System.Environment( getArgs )
import System.Info (os)
import System.Directory
import System.Process
import System.Exit
import System.Console.GetOpt
import System.IO

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

import System.FilePath((</>))

main = do user <- getAppUserDataDirectory "Cr.lock"
          locked <- doesFileExist user
          let run = myThreadId >>= \t -> withFile user WriteMode (do_program t)
                                           `finally` removeFile user
          if locked then do
                        putStrLn "There is already one instance of this program running."
                        putStrLn "Remove lock and start application? (Y/N)"
                        hFlush stdout
                        str <- getLine
                        if | str `elem` ["Y", "y"] -> run
                           | otherwise             -> return ()
                      else run

data Options = Options  {
    optPlatform  :: String,
    optBuild :: String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optPlatform = if | os `elem` ["win32", "mingw32", "cygwin32"] -> "Win"
                     | os `elem` ["darwin"] -> "Mac"
                     | otherwise -> "Linux"
        ,
    optBuild = go "last"
  }

do_program :: ThreadId -> Handle -> IO ()
do_program t h = let s = "Locked by thread: " ++ show t
                 in do  putStrLn s
                        hPutStr h s
                        args <- getArgs
                        let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) actions
                        let Options { optPlatform = platform,
                                    optBuild = build } = opts
                        build platform

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['d'] ["dartium"] (NoArg getDartium) "Get dartium",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number"
  ]
showHelp _ = do putStrLn $ usageInfo "Usage: Cr [optional things]" options
                exitWith ExitSuccess

showChromeVersion _ = do
    ls <- getLastVersionForPlatform "Win"
    printf "last: %s\n" ls  >> exitWith ExitSuccess
getDartium _ = getDart      >> exitWith ExitSuccess

getp arg opt = return opt { optPlatform = arg }
getb arg opt = return opt { optBuild = go arg }

data Config = Config { cr :: String
                     , installed  :: Int
    } deriving (Read, Show)
readConfig :: String -> Config
readConfig = read
writeConfig :: Config -> String
writeConfig = show

cSwrap = bracket_
     ( do
            putStrLn " ________________________________________________________ "
            putStrLn "          And who the hell do you think I've become?      "
            putStrLn "  Like the person inside, I've been opening up.           "
            putStrLn "                            I'm onto you. (I'm onto you.) "
            putStrLn " ________________________________________________________ "
    )( do
            putStrLn " ________________________________________________________ "
            putStrLn " Cut out your tongue and feed it to the liars.            "
            putStrLn "     Black hearts shed light on dying words.              "
            putStrLn "                                                          "
            putStrLn "                                 I wanna feel you burn.   "
            putStrLn " ________________________________________________________ "
            putStrLn ""
    )

go :: String -> String -> IO()
go bl pl = do
    let cfg = "Cr.cfg"
    config <- doesFileExist cfg >>= \isCfgEx ->
                if isCfgEx then readFile cfg >>= return . readConfig
                           else return Config{cr="Chromium", installed=0}
    case (cr config) of
     "Dart" -> getDartium()
     _      -> cSwrap $ do              -- Chromium --
        ls <- if bl == "last"
                then do putStrLn " -> Checking for the last version"
                        r <- try (getLastVersionForPlatform pl)
                                :: IO (Either SomeException String)
                        case r of
                            Left what -> do putStrLn $ show what
                                            return   $ show (installed config)
                            Right val -> return val
                else (return bl)
        let ils = read ls :: Int
        if (installed config) >= ils 
            then putStrLn " -> Installed version is newer or the same"
            else do let new_config = config{installed=ils}
                    let fname = "mini_installer.exe"
                    printf " -> Downloading %s\n" ls
                        >> getChromium pl ls fname

                    putStrLn " -> Installing"
                    pid <- runCommand fname
                    waitForProcess pid >>= \exitWith -> do
                        fileExist <- doesFileExist fname
                        when fileExist $ do
                            putStrLn " -> Clean Up"
                            removeFile fname
                        writeFile cfg $ writeConfig new_config

        putStrLn " ________________________________________________________ "
        putStrLn " -> Running"

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        getShellFolder >>= \shellfolder ->
            let chromium = shellfolder </> "Chromium\\Application\\chrome.exe"
            in createProcess (proc chromium []) >> return ()
#endif
