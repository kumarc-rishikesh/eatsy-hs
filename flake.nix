{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = ["x86_64-linux" "aarch64-darwin"];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }:
      {
        haskellProjects.default = {
                    
          basePackages = pkgs.haskellPackages;

          packages = {
            scotty.source = "0.22";
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
            jq
            azure-cli
          ];
        };


        #todo : figure out how to get ENV impurely into docker container
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
          };
      };
      
      packages.default = self'.packages.eatsy-hs;
      apps.default = self'.apps.eatsy-hs;
      
      };
    };
}
