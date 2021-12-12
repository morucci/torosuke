{ nixpkgs ? import <nixpkgs> { }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, brick, directory, http-client
    , http-client-tls, http-types, lib, mtl, optparse-generic, relude
    , safe-exceptions, scheduler, scientific, text, time, unordered-containers
    , vector, vty, witch }:
    mkDerivation {
      pname = "torosuke";
      version = "0.0.0.1";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        base
        brick
        directory
        http-client
        http-client-tls
        http-types
        mtl
        relude
        safe-exceptions
        scheduler
        scientific
        text
        time
        unordered-containers
        vector
        vty
        witch
      ];
      executableHaskellDepends = [ base mtl optparse-generic relude time ];
      homepage = "https://github.com/morucci/torosuke#readme";
      license = lib.licenses.agpl3Only;
    };

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { });

in if pkgs.lib.inNixShell then drv.env else drv
