{-# LANGUAGE MultiWayIf #-}

module InstallHelper
  ( cSwrap
  , getConfig
  , openConfig
  ) where

import Yaml

import System.Directory
import System.FilePath(takeDirectory, (</>))
import System.Info (os)
import System.Environment.Executable ( getExecutablePath )

import Control.Exception
import Control.Applicative

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
