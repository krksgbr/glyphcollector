# Development

A development environment is provided using nix-shell.

Requirements:
- Linux or MacOS
- [nix](https://nixos.org/nix/)

### Start the frontend development server

```bash
nix-shell
make dev
```


### Start the backend

```bash
cd backend
make stack2nix-output.nix ## sets up dependencies (only has to be done once)
nix-shell
make dev ## starts ghcid
```

This will take a long time on the first run, because a lot of dependencies will
be built from source. On subsequent runs this should take only a few seconds.

#### NOTE
When working on image processing related modules, it's better to re-compile the
backend when testing things out. To do that, run `make run` instead of
`make dev` to run the backend.

This is because the `make dev` command above runs the backend through `ghcid`, therefore the
code will be interpreted rather than compiled. This is nice for quickly
iterating without having to re-compile, but performance sensitive parts will be unusable.

### Run electron
From the root of the repo:
``` 
nix-shell
electron electron.js --dev
```

## Compile the sources and the run app
This can be useful if eg. you're on a non-debian based linux distro such as Arch.
Make sure [nix](https://nixos.org/nix/) is installed and properly configured (for example [on Arch](https://wiki.archlinux.org/index.php/Nix)).

Clone the repo and run these commands from the root of the glyphcollector directory:
```
nix-shell
make dist
electron .
```
