{ pkgs }:

rec {
  # likely-music haskellPackage
  likely-music-lib = likely-music-backend;
  likely-music-backend = pkgs.haskellPackages.callPackage ./likely-music-backend.nix { };

  likely-music-frontend = pkgs.callPackage ./web { };
}
