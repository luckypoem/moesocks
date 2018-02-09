{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.pkgs.haskellPackages.callPackage ./moesocks.nix {}
