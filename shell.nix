{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  affectionNeko = with haskellPackages; callPackage ({ mkDerivation
    , base, bytestring, clock, containers, glib, linear, monad-loops, monad-parallel
    , mtl, OpenGL, sdl2, stdenv, stm, text, uuid, vector }:
    mkDerivation {
      pname = "affection";
      version = "0.0.0.9";
      #src = pkgs.fetchFromGitHub {
      #  owner = "nek0";
      #  repo = "affection";
      #  rev = "33c99b8888328e4ed17f5c65ac49f5eab2645549";
      #  sha256 = "0psqxqj1a8l5fia49ay2pb72kjnw5i54m6dcmrpz5hi1654aznll";
      #};
      src = ../affection;
      revision = "1";
      isLibrary = true;
      libraryHaskellDepends = [
        base bytestring clock containers glib linear monad-loops
        monad-parallel mtl OpenGL sdl2 stm text uuid vector
      ];
      librarySystemDepends = [ ];
      libraryToolDepends = [  ];
      testHaskellDepends = [ ];
      homepage = "https://github.com/nek0/affection";
      description = "A simple Game Engine in Haskell using SDL";
      license = pkgs.stdenv.lib.licenses.gpl3;
      hydraPlatforms = pkgs.stdenv.lib.platforms.none;
    }) { };

  bullet = pkgs.bullet;

  shootNeko = with haskellPackages; callPackage ({ mkDerivation
  , base, inline-c, inline-c-cpp, linear
  , stdenv
  }:
  mkDerivation {
    pname = "shoot";
    version = "0.0.0.0";
    src = ../shoot;
    isLibrary = true;
    isExecutable = false;
    libraryHaskellDepends = [ base inline-c inline-c-cpp linear ];
    libraryPkgconfigDepends = [ bullet ];
    description = "Haskell bindings to bullet library";
    license = stdenv.lib.licenses.lgpl3;
  }) {};

  spatial = with haskellPackages; callPackage ({ mkDerivation
  , base, binary, cereal, ghc-prim, lens, linear
  , stdenv, TypeCompose
  }:
  mkDerivation {
    pname = "spatial-math";
    version = "0.5.0.0";
    src = ../spatial-math-0.5.0.0;
    libraryHaskellDepends = [
      base binary cereal ghc-prim lens linear TypeCompose
    ];
    description = "3d math including quaternions/euler angles/dcms and utility functions";
    license = stdenv.lib.licenses.bsd3;
  }) {};


  f = { mkDerivation, base, bytestring, GLUtil, linear
      , OpenGL, OpenGLRaw, optparse-applicative, random, sdl2
      , split, stdenv, vector, wavefront
      }:
      mkDerivation {
        pname = "hw";
        version = "0.0.0.0";
        src = ./.;
        configureFlags = [ "-fexamples" ];
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          affectionNeko base bytestring GLUtil linear OpenGL OpenGLRaw
          optparse-applicative random sdl2 shootNeko spatial split vector
          wavefront
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
