{
  description = "hfsnotify development shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            # GHC 9.10.3, the compiler for lts-24.62 in stack.yaml
            pkgs.haskell.compiler.ghc9103

            pkgs.stack
            pkgs.cabal-install

            # The test suite pulls in a dependency that needs zlib's C library
            pkgs.pkg-config
            pkgs.zlib
          ];
        };
      });
    };
}
