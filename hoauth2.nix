{ mkDerivation, aeson, base, binary, bytestring, containers
, exceptions, hashable, http-conduit, http-types, microlens, mtl
, mustache, parsec, scotty, stdenv, text, unordered-containers
, uri-bytestring, uri-bytestring-aeson, wai, wai-extra
, wai-middleware-static, warp
}:
mkDerivation {
  pname = "hoauth2";
  version = "1.16.0";
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
    aeson base binary bytestring containers hashable http-conduit
    http-types microlens mtl mustache parsec scotty text
    unordered-containers uri-bytestring wai wai-extra
    wai-middleware-static warp
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = stdenv.lib.licenses.bsd3;
}
