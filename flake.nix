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
        
        compilerVersion = "ghc925";

        # fix things
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            "${compilerVersion}" =
              pkgs.haskell.packages."${compilerVersion}".override {
                overrides = self: super: {
                  # On aarch64-darwin, this creates a cycle for some reason; didn't look too much into it.
                  ghcid = pkgs.haskell.lib.overrideCabal super.ghcid (drv: { enableSeparateBinOutput = false; });
                };

              };
          };
        };

        haskellPackages = haskell.packages.${compilerVersion};

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "internet";
      in {
        # we're not interested in building with Nix, just using it for deps
        packages.${system}.${packageName} = {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            cabal-install
            pkgs.zlib
            pkgs.darwin.apple_sdk.frameworks.CoreServices
            # pkgs.darwin.objc4.all
            pkgs.darwin.apple_sdk.frameworks.Cocoa
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}

