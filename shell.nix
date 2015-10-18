{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base, binary
      , bytestring, containers, cryptohash, hslogger, HsOpenSSL, iproute
      , lens, lens-aeson, mtl, network, optparse-applicative, random
      , stdenv, stm, strict, text, time, transformers
      , unordered-containers
      , nemesis
      , angel
      , cabal-install
      , halive
      , hasktags
      , ghc-mod
      }:
      mkDerivation {
        pname = "moesocks";
        version = "1.0.0.30";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson async attoparsec base binary bytestring containers cryptohash
          hslogger HsOpenSSL iproute lens lens-aeson mtl network
          optparse-applicative random stm strict text time transformers
          unordered-containers
          nemesis
          angel
          cabal-install
          halive
          hasktags
          ghc-mod
        ];
        homepage = "https://github.com/nfjinjing/moesocks";
        description = "A functional firewall killer";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
