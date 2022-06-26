{ mkDerivation, aeson, base, binary, bytestring, exceptions
, hashable, http-conduit, http-types, lib, microlens, text
, unordered-containers, uri-bytestring, uri-bytestring-aeson
}:
mkDerivation {
  pname = "hoauth2";
  version = "2.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring exceptions hashable http-conduit
    http-types microlens text unordered-containers uri-bytestring
    uri-bytestring-aeson
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = lib.licenses.bsd3;
}
