{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    unixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, unixpkgs }:
    let
      systems = [ "aarch64-darwin" ];
    in
    builtins.foldl'
      (ss: s:
        let
          pkgs = nixpkgs.legacyPackages."${s}";
          upkgs = unixpkgs.legacyPackages."${s}";
        in
        ss //
        {
          devShells."${s}".default = pkgs.mkShell {
            packages = [
              upkgs.cabal-install
              upkgs.ghc
              upkgs.haskellPackages.haskell-language-server
              upkgs.ormolu
            ];
          };
          formatter."${s}" = nixpkgs.legacyPackages."${s}".nixpkgs-fmt;
        })
      { }
      systems;
}
