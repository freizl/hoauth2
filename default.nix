let
  pkgs = import <nixpkgs> { };

in
  { hoauth2 = pkgs.haskellPackages.callPackage ./hoauth2/hoauth2.nix { } ;
    hoauth2-example = pkgs.haskellPackages.callPackage ./hoauth2-example/hoauth2-example.nix { } ;
  }

