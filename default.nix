{
  pkgs ? import <nixpkgs> {},
  withHLS ? false,
}: let
  hsPkgs = pkgs.haskell.packages.ghc912.override {
    overrides = self: super: {
      HaskellForMaths = pkgs.haskell.lib.doJailbreak (super.HaskellForMaths.overrideAttrs (old: { meta.broken = false; }));
      brick = pkgs.haskell.lib.doJailbreak super.brick;
      diagrams-lib = pkgs.haskell.lib.doJailbreak super.diagrams-lib;
      diagrams-svg = pkgs.haskell.lib.doJailbreak super.diagrams-svg;
    };
  };
in (hsPkgs.developPackage {
  name = "agate";
  root = builtins.fetchGit ./.;
}).overrideAttrs
  (old: {
    buildInputs =
      (old.buildInputs or [ ])
      ++ pkgs.lib.optional withHLS hsPkgs.haskell-language-server
      ++ (with pkgs; [
        cabal-install
        graphviz
        ghcid
      ]);
  })
