{-# LANGUAGE UnicodeSyntax, CPP #-}

import Text.Printf
import System.Process
import System.Exit

import Google

version = "0.0.1"
main = do
    printf "\n  Cr v.%s\n\n" version  {-  Intro  -}
    
    printf "\n ========== "
    printf "\n -> Checking flor last version"
    ls <- getLastVersionForPlatform "Win"
    
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
        getChar
#endif