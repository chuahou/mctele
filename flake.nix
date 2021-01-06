# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

{
  description = "Simple Telegram bot that notifies when player joins server";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-20.09";

    # hs-updates branch for HLS 0.7.1
    hs-updates.url =
      "github:nixos/nixpkgs?rev=a4530367944e55dbbd1d127123f3519b7483bc54";
  };

  outputs = { self, nixpkgs, hs-updates }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; overlays = [ hlsOverlay ]; };

      hlsOverlay = self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hsSelf: hsSuper: {
            haskell-language-server =
              (import hs-updates { inherit system; }).haskell-language-server;
          };
        };
      };

      mctele = pkgs.haskell.lib.dontHaddock
        (pkgs.haskellPackages.callCabal2nix "mctele" ./. {});

      mctele-static = pkgs.haskell.lib.overrideCabal mctele (old: {
        enableSharedExecutables = false;
        enableSharedLibraries   = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.glibc.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (_:
            { dontDisableStatic = true; })}/lib"
        ];
      });

    in rec {
      defaultPackage."${system}" = mctele-static;

      devShell."${system}" =
        (pkgs.haskell.lib.overrideCabal mctele (old: {
          buildTools = (old.buildTools or []) ++ (with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
          ]);
        })).env;
    };
}
