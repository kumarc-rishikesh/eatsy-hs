# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "eatsy-hs";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            scotty = jailbreakUnbreak haskellPackages.scotty_0_22;
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;
        
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            ghcid
            cabal-install
            zlib
            zlib.dev
            dbeaver
            beekeeper-studio
            haskellPackages.yesod-bin
            jq
          ];
          shellHook = ''
              export NEURELO_KEY=$(cat neurelo_key)
              export NEURELO_ENDPOINT="https://us-east-2.aws.neurelo.com"
          '';
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
