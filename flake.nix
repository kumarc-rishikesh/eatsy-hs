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
      perSystem = { self', system, lib, config, pkgs, ... }:{
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
              export NEURELO_KEY=$(cat neurelo_key)
              export NEURELO_ENDPOINT="https://us-east-2.aws.neurelo.com"
          '';
        };


# WIP add envs
        packages.dockerImage = pkgs.dockerTools.buildImage {
          name = "eatsy-hs";
          created = "now";
          tag = builtins.substring 0 9 (self'.rev or "dev");
          copyToRoot = pkgs.buildEnv {
            paths = with pkgs; [
              # writeNeureloKey
              coreutils
              bash
            ];
            name = "eatsy-hs-root";
            pathsToLink = [ "/bin" "/secrets"];
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
