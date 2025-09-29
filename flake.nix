{
  description = "internet";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        packageName = "internet";
      in {
        # we're not interested in building with Nix, just using it for deps
        packages.${system}.${packageName} = {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.zola 
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}

