{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    hdeps = {
      url = "github:LightAndLight/hdeps";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };
  outputs = { self, nixpkgs, flake-utils, hdeps }:
    {
      overlays.hdeps = import ./nix/generated/overlay.nix;
    } //
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.extend (import ./nix/generated/overlay.nix);
            })
          ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            haskellPackages.implicit-hie
            fd

            hdeps.packages.${system}.default
            cabal2nix
          ];
        };

        packages.default = pkgs.haskellPackages.callPackage ./temple.nix {};
      }
    );
}
