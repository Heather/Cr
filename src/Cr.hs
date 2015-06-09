{-# LANGUAGE MultiWayIf
  , LambdaCase
  , UnicodeSyntax
  , RankNTypes
  , KindSignatures
  #-}

import Installer

import System.Console.GetOpt
import System.Directory
import System.IO
import System.Info (os)
import System.Environment (getArgs)

import Control.Concurrent
import Control.Exception
import Control.Monad.Unicode

import Foreign.Storable (sizeOf)
import Data.Foldable.Unicode

main ∷ IO ()
main = do (actions, _, _) ← getOpt RequireOrder options <$> getArgs
          Options { optPlatform   = platform,   optBuild      = build
                  , optForce      = force,      optRun        = run
                  } ← foldl (≫=) (return defaultOptions) actions
          user   ← getAppUserDataDirectory "Cr.lock"
          locked ← doesFileExist user
          let gogo = build platform force run
              start = myThreadId ≫= \t → withFile user WriteMode (do_program gogo t)
                                             `finally` removeFile user
          if locked then do putStrLn "There is already one instance of this program running."
                            putStrLn "Remove lock and start application? (Y/N)"
                            hFlush stdout
                            getLine >>= \case w | w ∈ ["Y", "y"] → start
                                              w | w ∈ ["N", "n"] → return ()
                                              _ → return ()
                    else start

do_program ∷ IO () → ThreadId → Handle → IO ()
do_program gogo _ _ = gogo

data Options = Options
    { optPlatform  ∷ String,   optForce ∷ Bool
    , optRun ∷ Bool,           optBuild ∷ String →  Bool → Bool → IO()
    }

defaultOptions ∷ Options
defaultOptions = Options {
    optPlatform = if | os ∈ ["win32", "mingw32", "cygwin32"] →
                       if sizeOf (undefined :: Int) == 8 then "Win_x64"
                                                         else "Win"
                     | os ∈ ["darwin"] → "Mac"
                     | otherwise → "Linux"
    , optForce = False, optRun   = False
    , optBuild = install "last"
    }

options ∷ [OptDescr (Options → IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg (showHelp options)) "Display Help",
    Option ['l'] ["last"]    (NoArg showChromeVersion) "show last chromium version number",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['b'] ["build"]   (ReqArg getb "STRING") "build number",
    Option ['f'] ["force"]   (NoArg forceReinstall) "force reinstall even if same version is installed",
    Option ['r'] ["run"]     (NoArg justRun) "just run without updating"
    ]

getp :: ∀ (m :: * -> *). Monad m ⇒ String → Options → m Options
getp arg opt = return opt { optPlatform = arg }

getb :: ∀ (m :: * -> *). Monad m ⇒ String → Options → m Options
getb arg opt = return opt { optBuild = install arg }

forceReinstall :: ∀ (m :: * -> *). Monad m ⇒ Options → m Options
forceReinstall opt = return opt { optForce = True }

justRun :: ∀ (m :: * -> *). Monad m ⇒ Options → m Options
justRun opt = return opt { optRun = True }
