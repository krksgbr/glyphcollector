let
  inherit (builtins) fetchTarball fetchGit;


  compiler = "ghc864";
  overlays = [ (import ./backend/overlays.nix { inherit compiler; }) ];

  pkgs = import ./nix/pkgs.nix {
    inherit overlays;
  };


  inherit (pkgs) haskellPackages;
  backend = haskellPackages.callCabal2nix "glyphcollector" ./backend {};
in
{
  app = backend;
  shell =
    let
      all-hies = import (
        fetchGit {
          name = "all-hies";
          url = "https://github.com/Infinisil/all-hies.git";
          rev = "9540f6aaeb9520abfc30729dc003836b790441a0";
        }
      ) {};

      hie = all-hies.selection { selector = p: { "${compiler}" = p.${compiler}; }; };

      ghcid = pkgs.haskellPackages.callCabal2nix "ghcid" (
        fetchGit {
          name = "ghcid";
          url = "https://github.com/ndmitchell/ghcid.git";
          rev = "939b009fb9426501cf3aa546f1573d4ebfe6645d";
        }
      ) {};

      ormolu = (
        import (
          fetchGit {
            name = "ormolu";
            url = "https://github.com/tweag/ormolu.git";
            rev = "a7076c0f83e5c06ea9067b71171859fa2ba8afd9";
          }
        ) {
          inherit pkgs;
        }
      ).ormolu;
    in
      haskellPackages.shellFor {
        packages = p: [ backend ];
        buildInputs = with pkgs; [

          ## backend stuff
          cabal-install
          hie
          ghcid
          ormolu

          ## frontend stuff
          nodejs
          yarn
          elmPackages.elm
          elmPackages.elm-format

          ## required by electron-forge
          dpkg
          fakeroot
        ];
        withHoogle = true;
      };
}
