with import  ./nix/pkgs.nix {};
mkShell {
  ELM_HOME = (builtins.getEnv "PWD") + "/.elm";
  buildInputs = [
    electron_6
    nodejs
    yarn
    elmPackages.elm
    elmPackages.elm-format
    jq
    zip

    ## required by electron-forge
    dpkg
    fakeroot
  ];
}
