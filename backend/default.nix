let
  stack2nix-output-path = ./stack2nix-output.nix;
  cabalPackageName = "glyphcollector-backend";
  compiler = "ghc865"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix = builtins.fetchTarball {
    url    = "https://github.com/nh2/static-haskell-nix/archive/9a3411ea699da37d778a4bd602a03fc68e1fd5ae.tar.gz";
    sha256 = "0wr09q00abjwgrcph9114060a95slwp6p9qw1d6avfz08q6jazb1";
  };

  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2020-02-07T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  buildScript = pkgs.writeShellScript "stack2nix-and-build-script.sh" ''
    set -eu -o pipefail
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build -A package "$@"
  '';

in
  {
    package = static-stack2nix-builder.static_package;
    inherit pkgs;
    inherit buildScript;
    inherit stack2nix-script;
  }
