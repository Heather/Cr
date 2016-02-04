{-# LANGUAGE UnicodeSyntax #-}

import Shake.It.Off

main :: IO ()
main = shake $ do
  "clean" ∫ cabal ["clean"]

  buildPath </> "Cr.exe" ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "install" ◉ [buildPath </> "Cr.exe"] ∰
    cabal ["install"]

 where buildPath :: String
       buildPath = "dist/build/Cr"
