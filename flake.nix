{
    description = "Simple Algebraic Effects for Haskell";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-23.05";
    };

    outputs = { self, nixpkgs, ... }:
    let
        system = "x86_64-linux";

        pkgs = import nixpkgs { inherit system; };

        hspkgs = pkgs.haskell.packages.ghc96;

        haskell-effects = hspkgs.callCabal2nix "haskell-effects" ./. {};

        haskell-effects-env = hspkgs.shellFor
        {
            packages = _: [ haskell-effects ];

            nativeBuildInputs = [
                pkgs.haskell.packages.ghc96.cabal-install
                pkgs.haskell.packages.ghc96.haskell-language-server
            ];
        };
    in
    {
        packages.${system}.default = haskell-effects;
        devShells.${system}.default = haskell-effects-env;
    };
}
