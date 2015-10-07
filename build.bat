@echo off
cabal install --only-dependencies
cabal configure
cabal build
pause
