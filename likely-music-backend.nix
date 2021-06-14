{ mkDerivation, aeson, base, bytestring, containers, directory
, Euterpea, filepath, HCodecs, lib, PortMidi, process, random
, servant, servant-server, text, wai, wai-app-static, warp
}:
mkDerivation {
  pname = "likely-music";
  version = "0.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers Euterpea PortMidi random text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers directory Euterpea filepath
    HCodecs process random servant servant-server text wai
    wai-app-static warp
  ];
  license = lib.licenses.agpl3Only;
}
