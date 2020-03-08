let
  repo = import ./default.nix;
in
repo.package.env.overrideAttrs(oldAttrs: {
  buildInputs = with repo.pkgs;
    oldAttrs.buildInputs ++ [
      cabal-install
      stack
      ghcid
      zlib
      haskellPackages.hpack
    ];
})
