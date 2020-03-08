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
