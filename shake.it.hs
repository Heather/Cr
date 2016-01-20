import System.Exit
import System.Process

import Data.List

import Control.Monad

checkExitCode :: ExitCode -> IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure code) =
    error $ "failed with exit code: " ++ (show code)

cabal :: [String] -> IO ()
cabal a = rawSystem "cabal" a >>= checkExitCode

main :: IO ()
main = do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]
