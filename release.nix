{ project ? import ./default.nix {} }:
  {
    inherit (project.shells) ghc;
    agate = project.hsPkgs.agate.components.library;
  }
