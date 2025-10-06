{
  pkgs ? import <nixpkgs> {},
  withHLS ? false,
}: let
  hsPkgs = pkgs.haskell.packages.ghc912.override {
    overrides = self: super: {
      HaskellForMaths = pkgs.haskell.lib.doJailbreak (super.HaskellForMaths.overrideAttrs (old: { meta.broken = false; }));
    };
  };
in (hsPkgs.developPackage {
  name = "agate";
  root = builtins.fetchGit ./.;
}).overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ pkgs.lib.optional withHLS hsPkgs.haskell-language-server;
})
