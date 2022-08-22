##
## Learning resources
## - https://github.com/Gabriella439/haskell-nix
##
##

{
  compiler ? "ghc924"
} :

let
  pkgs = import <nixpkgs> { };
  haskellPkg = pkgs.haskell.packages.${compiler};

in
  {
    hoauth2 = haskellPkg.callPackage ./hoauth2/hoauth2.nix { } ;
    hoauth2-example = haskellPkg.callPackage ./hoauth2-example/hoauth2-example.nix { } ;
  }

