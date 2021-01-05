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

      mcjoin = pkgs.haskell.lib.dontHaddock
        (pkgs.haskellPackages.callCabal2nix "mcjoin" ./. {});

    in rec {
      defaultPackage."${system}" = mcjoin;

      devShell."${system}" =
        (pkgs.haskell.lib.overrideCabal mcjoin (old: {
          buildTools = (old.buildTools or []) ++ (with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
          ]);
        })).env;
    };
}
