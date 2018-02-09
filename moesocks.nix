{ mkDerivation, aeson, async, attoparsec, base, binary, bytestring
, containers, cryptohash, hslogger, HsOpenSSL, iproute, lens
, lens-aeson, mtl, network, optparse-applicative, random, stdenv
, stm, strict, text, time, transformers, unordered-containers
, pkgs
}:

let
  nemesis = pkgs.callPackage /home/jinjing/scm/haskell/nemesis {}
; in

mkDerivation {
  pname = "moesocks";
  version = "1.0.0.44";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base binary bytestring containers cryptohash
    hslogger HsOpenSSL iproute lens lens-aeson mtl network
    optparse-applicative random stm strict text time transformers
    unordered-containers
    nemesis
  ];
  homepage = "https://github.com/nfjinjing/moesocks";
  description = "A functional firewall killer";
  license = stdenv.lib.licenses.asl20;
}
