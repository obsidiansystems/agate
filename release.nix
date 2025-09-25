{ project ? import ./default.nix {} }:
  {
    inherit (project.shells) ghc;
  }
