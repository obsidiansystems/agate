{
  system ? builtins.currentSystem,
  reflex-platform ? import ./dep/reflex-platform/default.nix { inherit system; },

  nix-thunk ? import ../dep/nix-thunk { },
  thunkInputs ? []
}:
reflex-platform.project ({ pkgs, thunkSource, ... }: {
  name = "agate";
  src = ./.;

  compiler-nix-name = "ghc8107Splices";
  ghcjs-compiler-nix-name = "ghcjs8107JSString";
  #inputThunks = thunkInputs ++ pkgs.obsidianCompilers.thunkSets.aeson-1541;
  shells = ps: with ps; [ agate ];

  nativeBuildInputs = [pkgs.graphviz];

  shellTools = {
    cabal = {
      version = "3.8.1.0";
      index-state = "2023-03-04T00:00:00Z";
    };
    haskell-language-server = {
      version = "1.8.0.0";
      compiler-nix-name = "ghc8107";
      index-state = "2023-03-04T00:00:00Z";
    };
    ghcid = {
      version = "0.8.9";
      index-state = "2023-03-04T00:00:00Z";
    };
  };

  overrides = [ ];
})
