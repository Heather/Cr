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
    optForce :: Bool,
    optRun :: Bool,
    optBuild :: String ->  Bool -> Bool -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optPlatform = if | os `elem` ["win32", "mingw32", "cygwin32"] -> "Win"
                     | os `elem` ["darwin"] -> "Mac"
                     | otherwise -> "Linux"
        ,
    optForce = False,
    optRun   = False,
    optBuild = go "last"
  }

do_program :: ThreadId -> Handle -> IO ()
do_program t h = let s = "Locked by thread: " ++ show t
                 in do  putStrLn s
                        hPutStr h s
                        args <- getArgs
                        let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) actions
                        let Options { optPlatform   = platform,
                                      optBuild      = build,
                                      optForce      = force,
                                      optRun        = run } = opts
                        build platform force run

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['m'] ["mozilla"] (NoArg fireFox) "Install Firefox UX :)",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number",
    Option ['f'] ["force"]   (NoArg forceReinstall) "force reinstall even if same version is installed",
    Option ['r'] ["run"]     (NoArg justRun) "just run without updating"
  ]
showV _    =    printf "Cr 0.3.0" >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: Cr [optional things]" options
                exitWith ExitSuccess

showChromeVersion _ = do
    ls <- getLastVersionForPlatform "Win"
    printf "last: %s\n" ls  >> exitWith ExitSuccess

getp arg opt        = return opt { optPlatform = arg }
getb arg opt        = return opt { optBuild = go arg }
forceReinstall opt  = return opt { optForce = True }
justRun opt         = return opt { optRun = True }

data Config = Config { installed  :: Int
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
    
fireFox _ = do
    let basedir = "D:\\Program Files\\UX" -- TODO --
        ux      = basedir </> "firefox.exe"
    exeExist <- doesFileExist ux
    if exeExist
        then 
            let updater = basedir </> "updater.exe"
            in createProcess (proc updater []) >> return ()
        else cSwrap $ do
            let ls = "29.0a1"
                ils = read ls :: Int
                fname = "firefox-" ++ ls ++ ".en-US.win32.installer.exe"
            printf " -> Getting %s\n" ls
                >> getUX fname
            putStrLn " -> Installing"
            pid <- runCommand fname
            waitForProcess pid >>= \exitWith -> do
                fileExist <- doesFileExist fname
                when fileExist $ do
                    putStrLn " -> Clean Up"
                    removeFile fname
    when exeExist $ do
        printf " -> Running"
            >> createProcess (proc ux [])
            >> return ()
    exitWith ExitSuccess

go :: String -> String -> Bool -> Bool -> IO()
go bl pl force run = do
    when (not run) $ do
        let cfg = "Cr.cfg"
        config <- doesFileExist cfg >>= \isCfgEx ->
                    if isCfgEx then readFile cfg >>= return . readConfig
                               else return Config{installed=0}
        cSwrap $ do
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
            if (installed config) >= ils && not force
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