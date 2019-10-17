{ mkDerivation, aeson, base, binary, bytestring, exceptions
, http-conduit, http-types, microlens, stdenv, text
, unordered-containers, uri-bytestring, uri-bytestring-aeson
}:
mkDerivation {
  pname = "hoauth2";
  version = "1.9.0";
  sha256 = "1br1g6xp0s73aj8nsx3rjdpji3lvp19b4xaxhn87fqbnnhczg39z";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring exceptions http-conduit http-types
    microlens text unordered-containers uri-bytestring
    uri-bytestring-aeson
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = stdenv.lib.licenses.bsd3;
}
