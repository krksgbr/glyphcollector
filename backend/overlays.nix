{ compiler }: self: super:
let
  inherit (self.haskell.lib) dontCheck dontHaddock;
  inherit (builtins) fetchTarBall fetchGit;

in
{
  haskellPackages = super.haskell.packages.${compiler}.override {
    overrides = hself: hsuper:
      let
        inherit (hself) callPackage callCabal2nix callHackage;

        schedulerSrc = fetchGit {
          name = "scheduler";
          url = "https://github.com/lehins/haskell-scheduler";
          rev = "b503f1c76f8f6e9d69ad96807a19347ad6ee98fc";
        };

        massivSrc = fetchGit {
          name = "massiv";
          url = "https://github.com/lehins/massiv.git";
          rev = "3809bf8885f7de4d41fc1a2357888e5e1f0669b3";
        };

        elmStreetSrc = fetchGit {
          name = "elm-street";
          url = "https://github.com/Holmusk/elm-street.git";
          rev = "69a574055c281e781da65f31b6feba075a4f9728";
        };
      in
        {
          fib = callHackage "fib" "0.1" {};
          scheduler = dontCheck (callCabal2nix "scheduler" "${schedulerSrc}/scheduler" {});
          massiv = dontCheck (callCabal2nix "massiv" "${massivSrc}/massiv" {});
          massiv-io = dontCheck (callCabal2nix "massiv-io" "${massivSrc}/massiv-io" {});
          elm-street = dontCheck (callCabal2nix "elm-street" elmStreetSrc {});
        };
  };
}
