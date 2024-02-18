{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };
      drv = pkgs.haskellPackages.callCabal2nix "writing-a-c-compiler" ./. {};
      shell = pkgs.mkShell {
        packages = [ pkgs.gcc pkgs.python3 ];
        inputsFrom = [ drv ];
      };
    in {
      packages.writing-a-c-compiler = drv;
      defaultPackage = drv;
      devShell = shell;
    });
}
