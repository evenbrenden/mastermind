{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hspec, hspec-discover, QuickCheck
      , random, stdenv, transformers, trifecta
      }:
      mkDerivation {
        pname = "mastermind";
        version = "1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base random trifecta ];
        executableHaskellDepends = [ base transformers trifecta ];
        testHaskellDepends = [ base hspec hspec-discover QuickCheck ];
        testToolDepends = [ hspec-discover ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
