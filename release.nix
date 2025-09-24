{ reflex-platform ? import ./dep/reflex-platform
, supportedSystems ? [ "x86_64-linux" ] # "x86_64-darwin"
}:
let
  rp = reflex-platform {};
  pkgs = rp.nixpkgs;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  commonOverrides = self: super: {
    HaskellForMaths = haskellLib.doJailbreak (self.callHackageDirect {
        pkg = "HaskellForMaths";
        ver = "0.4.10";
        sha256 = "sha256-yPNKgyZTG5iJ2WYE6doskGripTDgtouTN2Np4tjQBHE=";
    } {});
  };
  ghcs = lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpGhc = rp.ghc.override {
      overrides = commonOverrides;
    };
    nixGhc945 = (import ./dep/nixpkgs { inherit system; }).haskell.packages.ghc945.override {
      overrides = self: super: commonOverrides self super // {
      };
    };
    nixGhc961 = (import ./dep/nixpkgs { inherit system; }).haskell.packages.ghc961.override {
      overrides = self: super: commonOverrides self super // {
      };
    };
  in
  {
    recurseForDerivations = true;
    ghc810 = rpGhc.callCabal2nix "agate" (import ./src.nix) {};
    ghc945 = nixGhc945.callCabal2nix "agate" (import ./src.nix) {};
    ghc961 = nixGhc961.callCabal2nix "agate" (import ./src.nix) {};
  });
  in
    ghcs
