{ mkDerivation, aeson, base, binary, bytestring, containers
, data-default, exceptions, http-conduit, http-types, lib
, microlens, text, transformers, uri-bytestring
, uri-bytestring-aeson
}:
mkDerivation {
  pname = "hoauth2";
  version = "2.5.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring containers data-default exceptions
    http-conduit http-types microlens text transformers uri-bytestring
    uri-bytestring-aeson
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = lib.licenses.bsd3;
}
