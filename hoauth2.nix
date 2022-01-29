{ mkDerivation, aeson, base, binary, bytestring, exceptions
, hashable, http-conduit, http-types, lib, microlens, mustache
, parsec, scotty, text, unordered-containers, uri-bytestring
, uri-bytestring-aeson, wai, wai-middleware-static, warp
}:
mkDerivation {
  pname = "hoauth2";
  version = "1.16.1";
  src = ./.;
  configureFlags = [ "-ftest" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring exceptions http-conduit http-types
    microlens text unordered-containers uri-bytestring
    uri-bytestring-aeson
  ];
  executableHaskellDepends = [
    aeson base binary bytestring hashable http-conduit http-types
    microlens mustache parsec scotty text unordered-containers
    uri-bytestring wai wai-middleware-static warp
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = lib.licenses.bsd3;
}
