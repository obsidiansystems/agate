# As of GHC 9.12, we can't yet use `:main` in multi-component GHCI, so we have to run _all_ the tests.
ghcid "--command=cabal repl --enable-multi-repl tests lib:agate lib:diagrams-svg" -T Main.mainAcceptAll -W
