{ mkDerivation, aeson, base, bytestring, containers, data-default
, directory, hoauth2, http-conduit, http-types, lib, microlens
, mustache, parsec, scotty, text, transformers
, unordered-containers, uri-bytestring, wai, wai-middleware-static
, warp
}:
mkDerivation {
  pname = "hoauth2-demo";
  version = "1.4.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default directory hoauth2
    http-conduit http-types microlens mustache parsec scotty text
    transformers unordered-containers uri-bytestring wai
    wai-middleware-static warp
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "OAuth2 example";
  license = lib.licenses.bsd3;
}
