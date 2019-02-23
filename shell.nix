{
    pkgs ? import <nixpkgs> {},
    stdenv ? pkgs.stdenv
}:

stdenv.mkDerivation {
    name = "transferwise-shell";

    buildInputs = with pkgs; [
        zlib.dev
        ghc
        haskellPackages.ghcid
    ];

    inherit (pkgs) zlib;

    shellHook = "export LD_LIBRARY_PATH=${pkgs.zlib}/lib";
}
