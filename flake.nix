# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

{
  description = "Simple Telegram bot that notifies when player joins server";

  inputs = {
    # latest hs-updates branch for newest HLS
    hs-updates.url = "nixpkgs/haskell-updates";
  };

  outputs = { self, nixpkgs, hs-updates }:
    let
      system = "x86_64-linux";
      pkgs   = import nixpkgs { inherit system; };

      mcjoinhs = pkgs.haskell.lib.dontHaddock
        (pkgs.haskellPackages.callCabal2nix "mcjoinhs" ./. {});

    in rec {
      defaultPackage."${system}" = mcjoinhs;
    };
}
