#!/usr/bin/env bash
# Runs the test suite on file changes. Passes options to Tasty, e.g. `-p SIR.Combined --accept`.
# Note that we have to use `:set` instead of `:main` due to limitations in GHCI multi-component support, as of GHC 9.12.
ghcid "--command=cabal repl --enable-multi-repl tests lib:agate" -s ":set args --color=always $*" -T Main.main -W
