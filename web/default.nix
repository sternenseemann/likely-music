{ stdenv, fetchFromGitHub, callPackage
, bash, yarn
, production ? true }:

let
  target = if production then "prod" else "dev";
  pkgInfo = stdenv.lib.importJSON ./package.json;
  src = yarn2nix-lib.removePrefixes [ "node_modules" "dist" ] ./.;
#  yarn2nixSrc = /home/lukas/src/nix/yarn2nix;
  yarn2nixSrc = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = "yarn2nix";
    rev    = "ea1f8f972a15ce2134bd113ee167d520d347be09";
    sha256 = "0lqi9wlbhwvhqi4lqagirg08q1zpxa55rc13ajypjgqrmfcr9aaq";
  };
  yarn2nix = import yarn2nixSrc { };
  yarn2nix-lib = callPackage (yarn2nixSrc + /nix-lib) {
    inherit yarn2nix;
  };
  deps = yarn2nix-lib.buildCallDeps { yarnLock = ./yarn.lock; };
  template = yarn2nix-lib.buildTemplate { packageJson = ./package.json; };
  calledTemplate =
    yarn2nix-lib.callTemplate template
      (yarn2nix-lib.buildNodeDeps deps);
  node_modules = yarn2nix-lib.linkNodeDeps {
    inherit (pkgInfo) name;
    dependencies = calledTemplate.nodeBuildInputs;
  };

in

stdenv.mkDerivation rec {
  pname = pkgInfo.name;
  inherit (pkgInfo) version;
  inherit src;

  buildInputs = [ yarn ];

  phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];

  postPatch = ''
    substituteInPlace ./package.json \
      --replace node_modules "${node_modules}"
  '';

  NODE_PATH = "${node_modules}";

  buildPhase = ''
    # temporary $HOME for yarn config
    export HOME=$(pwd)/yarn_home
    mkdir -p $HOME
    yarn config --offline set script-shell "${bash}/bin/bash"

    # binaries of deps.nix
    export PATH="${node_modules}/.bin:$PATH"

    # browserify won't look in NODE_PATH for modules
    ln -s ${node_modules} node_modules
    yarn run --offline build:assets
    yarn run --offline build:${target}
  '';

  installPhase = ''
    mkdir -p $out/share/
    cp -R dist $out/share/${pname}
  '';

  meta = calledTemplate.meta // {
    description = "Frontend of likely music, a probabilistic music notation software";
    homepage = "https://github.com/sternenseemann/likely-music";
  };
}
