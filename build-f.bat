@echo off
cabal install --only-dependencies --force-reinstalls
cabal configure
cabal build
pause
