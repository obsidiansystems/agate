{
  pkgs ? import ./dep/nixpkgs {},
  withHLS ? false,
}: let
  hsPkgs = pkgs.haskell.packages.ghc912.override {
    overrides = self: super: {
      HaskellForMaths = pkgs.haskell.lib.doJailbreak (super.HaskellForMaths.overrideAttrs (old: { meta.broken = false; }));
      brick = pkgs.haskell.lib.doJailbreak super.brick;
      diagrams-lib = pkgs.haskell.lib.doJailbreak super.diagrams-lib;
      diagrams-rasterific = pkgs.haskell.lib.doJailbreak super.diagrams-rasterific;
      diagrams-svg = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "diagrams-svg" (pkgs.fetchFromGitHub {
        owner = "georgefst";
        repo = "diagrams-svg";
        rev = "animations";
        hash = "sha256-AUwvzlriILQDgsWL+mlKYfP5+DSBEyXHYenUeJTdtPw=";
      }) {});
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
