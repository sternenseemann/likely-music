{ stdenv, fetchFromGitHub, callPackage, yarn
, production ? true }:

let
  target = if production then "prod" else "dev";
  pkgInfo = stdenv.lib.importJSON ./package.json;
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
  yarnFlags = stdenv.lib.escapeShellArgs [ "--offline" "--frozen-lockfile" ];

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

  # make sure vis-network find vis-data
  NODE_PATH = "${node_modules}";

  buildPhase = ''
    # browserify won't look in NODE_PATH for modules
    # due to the symlink yarn will also add node_modules/.bin
    # to PATH, so we don't have to do it
    ln -s ${node_modules} node_modules
    yarn ${yarnFlags} run build:assets
    yarn ${yarnFlags} run build:${target}
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
