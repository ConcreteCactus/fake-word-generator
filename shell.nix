{pkgs ? import <nixpkgs> {}}:
with pkgs;
let
  ghc = haskell.packages."ghc8107".ghcWithPackages (ps: with ps; [
          random
        ]);
in
stdenv.mkDerivation {
  name = "haskell-env";
  buildInputs = [ ghc ];
}
