{-# LANGUAGE UnicodeSyntax, CPP #-}

import Data.List
import Data.Function
import Data.Char
import Data.IORef
import Data.Maybe

import Control.Monad
import Control.Applicative

import Text.Printf

import Google

version = "0.0.1"
main = do

    printf "\n  Cr v.%s\n\n" version  {-  Intro  -}
   
    ls <- getLastVersionForPlatform "Win"
    
    printf "\n ========== "
    printf "\n -> Getting %s" ls

    getChromium "Win" ls
    
    printf "\n -> Done"
    printf "\n ========== "
    printf "\n"
    
    -- > Wait for keypress (Only for windows)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    getChar
#endif
    -- >