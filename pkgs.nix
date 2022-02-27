{ pkgs, napalm }:

rec {
  # likely-music haskellPackage
  likely-music-lib = likely-music-backend;
  likely-music-backend = pkgs.haskellPackages.callPackage ./likely-music-backend.nix { };

  likely-music-frontend = pkgs.callPackage ./web {
    inherit napalm;
  };

  # executable wrapper around everything with correct paths
  likely-music = pkgs.runCommand "likely-music" { } ''
    mkdir -p $out/bin
    source "${pkgs.dieHook}/nix-support/setup-hook"
    source "${pkgs.makeWrapper}/nix-support/setup-hook"
    makeWrapper "${likely-music-backend}/bin/likely-music-backend" "$out/bin/likely-music" \
      --argv0 likely-music \
      --set LIKELY_MUSIC_FRONTEND "${likely-music-frontend}/share/likely-music-frontend" \
      --set LIKELY_MUSIC_SYNTH "${fluidsynth-wrapper}/bin/fluidsynth-wrapper"
    '';

  fluidsynth-wrapper = pkgs.writeTextFile {
    name = "fluidsynth-wrapper";
    executable = true;
    destination = "/bin/fluidsynth-wrapper";
    text = ''
      #!${pkgs.bash}/bin/bash
      # fluidsynth-wrapper IN.mid OUT.wav
      if [ -z "$1" -o -z "$2" ]; then
        echo "$0: missing file parameter(s)" >&2
        exit 1
      fi
      ${pkgs.fluidsynth}/bin/fluidsynth -a file -i ${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM2-2.sf2 -F "$2" "$1"
    '';
  };
}
