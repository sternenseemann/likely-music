{ pkgs ? import <nixpkgs> {}; }:

import ./pkgs.nix { inherit pkgs; }
