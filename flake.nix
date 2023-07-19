{
  description = "heckle";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      defaultPackage = pkgs.stdenv.mkDerivation {};
      # apps.heckle = flake-utils.lib.mkApp { drv = packages.heckle; };
      # defaultApp = apps.heckle;
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          emacs
          nodePackages_latest.eask
        ];
      };
    });
}
