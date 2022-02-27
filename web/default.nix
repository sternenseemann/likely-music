{ stdenv, lib, esbuild, napalm, nix-gitignore }:

let
  src = builtins.path {
    name = "likely-music-frontend-source";
    path = ./.;
    filter = nix-gitignore.gitignoreFilter (builtins.readFile ../.gitignore) ./..;
  };
in

napalm.buildPackage src {
  nativeBuildInputs = [
    esbuild
  ];

  npmCommands = "make";

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share
    mv dist $out/share/likely-music-frontend
    runHook postInstall
  '';

  meta = {
    description = "Frontend of likely music, a probabilistic music notation software";
    homepage = "https://github.com/sternenseemann/likely-music";
    license = lib.licenses.agpl3Only;
  };
}
