{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  , RankNTypes
  #-}

module InstallHelper
  ( cSwrap
  , getConfig
  , openConfig
  ) where

import Yaml

import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.Environment.Executable (getExecutablePath)

import Control.Exception

import Prelude.Unicode

cSwrap :: ∀ γ. IO γ → IO γ
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

getConfig ∷ IO String
getConfig =
    if | os ∈ ["win32", "mingw32", "cygwin32"] →
          ((</> "Cr.yml") . takeDirectory <$> getExecutablePath)
       | otherwise → return "/etc/Cr.yml"

condM :: Monad m => [(m Bool, m a)] → m a
condM ((test,action) : rest) = test >>= \t → if t then action
                                                  else condM rest

openConfig ∷ String → IO Config
openConfig ψ =
  condM [ (doesFileExist ψ, yDecode ψ ∷ IO Config)
        , (return True
                , return Config { works = "0"
                                , installed = "0"
                                , autoclose = False
                                })
        ]
