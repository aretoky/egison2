#!/bin/sh

ghc-pkg unregister terminfo
cabal install --reinstall -p --enable-executable-profiling terminfo
ghc-pkg unregister haskeline
cabal install --reinstall -p --enable-executable-profiling haskeline
ghc-pkg unregister containers
cabal install --reinstall -p --enable-executable-profiling containers
ghc-pkg unregister ghc-paths
cabal install --reinstall -p --enable-executable-profiling ghc-paths
ghc-pkg unregister utf8-string
cabal install --reinstall -p --enable-executable-profiling utf8-string

cabal install --reinstall -p --enable-executable-profiling --ghc-option=-auto-all
