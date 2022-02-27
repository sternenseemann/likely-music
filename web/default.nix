{ stdenv, lib, fetchFromGitHub, callPackage, esbuild }:

let
  pkgInfo = lib.importJSON ./package.json;
  src = yarn2nix-lib.removePrefixes [ "node_modules" "dist" ] ./.;
#  yarn2nixSrc = /home/lukas/src/nix/yarn2nix;
  yarn2nixSrc = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = "yarn2nix";
    rev    = "b0825bbe4b40f39763d53ba0044431a44b5f25cf";
    sha256 = "1q8wc5rnb00xwzcqsgb6wfkmymipf2bv1g2i33l5wyadp0hd18cp";
  };
  yarn2nix = import yarn2nixSrc { };
  yarn2nix-lib = yarn2nix.nixLib;
  deps = yarn2nix-lib.callYarnLock ./yarn.lock { };
  template = yarn2nix-lib.callPackageJson ./package.json { };
  calledTemplate = template (yarn2nix-lib.buildNodeDeps deps);
  node_modules = yarn2nix-lib.linkNodeDeps {
    inherit (pkgInfo) name;
    dependencies = calledTemplate.nodeBuildInputs;
  };

in

stdenv.mkDerivation rec {
  pname = pkgInfo.name;
  inherit (pkgInfo) version;
  inherit src;

  nativeBuildInputs = [ esbuild ];

  makeFlags = [
    "OFFLINE=true"
  ];

  NODE_PATH = node_modules;
  preBuild = ''
    ln -s ${node_modules} node_modules
  '';

  installPhase = ''
    mkdir -p $out/share/
    mv dist $out/share/${pname}
  '';

  meta = calledTemplate.meta // {
    description = "Frontend of likely music, a probabilistic music notation software";
    homepage = "https://github.com/sternenseemann/likely-music";
  };
}
