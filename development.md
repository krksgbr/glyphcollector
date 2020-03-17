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
nix-shell
make dev
```

This will take a long time on the first run, because a lot of dependencies will
be built from source. On subsequent runs this should take only a few seconds.

### Run electron
From the root of the repo:
``` 
nix-shell
electron electron.js
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
