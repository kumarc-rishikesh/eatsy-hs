{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = ["x86_64-linux"];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }:
      {
        haskellProjects.default = {
                    
          basePackages = pkgs.haskellPackages;

          packages = {
            scotty.source = "0.22"; # Hackage version
          };

          devShell = {
            hlsCheck.enable = false;
          };

          autoWire = [ "packages" "apps" "checks" ]; 
        };

        devShells.default = pkgs.mkShell {
          name = "eatsy-hs devShell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            ghcid
            cabal-install
            zlib
            zlib.dev
            dbeaver-bin
            beekeeper-studio
            haskellPackages.yesod-bin
            jq
            azure-cli
          ];
          shellHook = '' 
              source ./Keys.env
              export NEURELO_KEY=$NEURELO_KEY
              export NEURELO_ENDPOINT=$NEURELO_ENDPOINT
              '';
        };


# WIP add envs
        packages.dockerImage = pkgs.dockerTools.buildImage {
          name = "eatsy-hs";
          created = "now";
          tag = builtins.substring 0 9 (self'.rev or "dev");
          copyToRoot = pkgs.buildEnv {
            paths = with pkgs; [
              coreutils
              bash
            ];
            name = "eatsy-hs-root";
          };        
          config = {
            Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
            Env = [
              ''NEURELO_KEY=${builtins.getEnv "NEURELO_KEY"}''
              ''NEURELO_ENDPOINT=${builtins.getEnv "NEURELO_ENDPOINT"}''
            ];
          };
      };
      
      packages.default = self'.packages.eatsy-hs;
      apps.default = self'.apps.eatsy-hs;
      
      };
    };
}
