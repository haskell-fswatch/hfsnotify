{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.haskell.compiler.ghc9103

            pkgs.stack
            pkgs.cabal-install

            pkgs.pkg-config
            pkgs.zlib
          ];
        };
      });
    };
}
