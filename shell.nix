with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, criterion, stdenv, tasty
             , tasty-hunit, tasty-quickcheck
             }:
             mkDerivation {
               pname = "streaming-histogram";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base containers criterion ];
               testDepends = [
                 base containers tasty tasty-hunit tasty-quickcheck
               ];
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
