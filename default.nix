let
  napalmSrc = builtins.fetchTarball {
    url = "https://github.com/nix-community/napalm/archive/master.tar.gz";
  };
in

{ pkgs ? import <nixpkgs> {}
, napalm ? pkgs.callPackage napalmSrc {}
}:

import ./pkgs.nix { inherit pkgs napalm; }
