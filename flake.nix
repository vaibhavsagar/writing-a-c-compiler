{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.cabal-gild-src.url = "github:tfausak/cabal-gild";
  inputs.cabal-gild-src.flake = false;
  # inputs.nix-filter.url = "github:numtide/nix-filter";

  inputs.flake-compat.url = "github:edolstra/flake-compat";
  outputs = {nixpkgs, flake-utils, cabal-gild-src, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };
      drv = pkgs.haskellPackages.callCabal2nix "writing-a-c-compiler" ./. {};
      cabal-gild = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix "cabal-gild" cabal-gild-src {});
      shell = pkgs.mkShell {
        packages = [ pkgs.gcc pkgs.python3 cabal-gild ];
        inputsFrom = [ drv ];
      };
    in {
      packages.writing-a-c-compiler = drv;
      defaultPackage = drv;
      devShell = shell;
    });
}
