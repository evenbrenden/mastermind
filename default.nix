{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c3792f6cddc71ba348e018406f125503d088c405.tar.gz") {}
, compiler ? "default"
, doBenchmark ? false }:

let

  f = { mkDerivation, base, hspec, hspec-discover, lib, QuickCheck
      , quickspec, random, transformers, trifecta
      }:
      mkDerivation {
        pname = "mastermind";
        version = "1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base random ];
        executableHaskellDepends = [ base transformers trifecta ];
        testHaskellDepends = [
          base hspec hspec-discover QuickCheck quickspec
        ];
        testToolDepends = [ hspec-discover ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
