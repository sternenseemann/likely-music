{ mkDerivation, aeson, base, bytestring, containers, directory
, Euterpea, filepath, HCodecs, PortMidi, process, random, servant
, servant-server, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "likely-music";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers Euterpea PortMidi random text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers directory Euterpea filepath
    HCodecs process random servant servant-server text wai warp
  ];
  license = stdenv.lib.licenses.agpl3;
}
