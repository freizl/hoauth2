let
  pkgs = import <nixpkgs> { };

in
  { hoauth2 = pkgs.haskellPackages.callPackage ./hoauth2.nix { } ;
  }

