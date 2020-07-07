{ mkDerivation, base, containers, justified-containers, stdenv }:
mkDerivation {
  pname = "logic-justified-evaluator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers justified-containers ];
  executableHaskellDepends = [
    base containers justified-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
