with import  ./nix/pkgs.nix {};
mkShell {
  buildInputs = [
    electron_6
    nodejs
    yarn
    elmPackages.elm
    elmPackages.elm-format
    jq

    ## required by electron-forge
    dpkg
    fakeroot
  ];
}
