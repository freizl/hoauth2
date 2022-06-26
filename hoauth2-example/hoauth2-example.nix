{ mkDerivation, aeson, base, binary, bytestring, directory
, hashable, hoauth2, http-conduit, http-types, lib, microlens
, mustache, parsec, scotty, text, unordered-containers
, uri-bytestring, wai, wai-middleware-static, warp
}:
mkDerivation {
  pname = "hoauth2-example";
  version = "1.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson base binary bytestring directory hashable hoauth2
    http-conduit http-types microlens mustache parsec scotty text
    unordered-containers uri-bytestring wai wai-middleware-static warp
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "OAuth2 example";
  license = lib.licenses.bsd3;
}
