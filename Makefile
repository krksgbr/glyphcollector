NODE_BIN = $(PWD)/node_modules/.bin
OS=$(shell uname -s)
VERSION=0.2.0

all:

ifeq ($(OS), Darwin)
all: macos
else
all: linux
endif

COMMON_DEPS = \
	clean \
	node_modules \
	dist \
	frontend \
	version

.PHONY: version
version:
	@jq '.version = "$(VERSION)"' package.json > package.json.tmp
	@mv package.json.tmp package.json

.PHONY: linux
linux: $(COMMON_DEPS)
	@cp backend/result/bin/gc-core dist
	@$(NODE_BIN)/electron-forge make

.PHONY: macos
macos: $(COMMON_DEPS)
	@cd backend && cp `stack path --local-install-root`/bin/gc-core ../dist
	@$(NODE_BIN)/electron-forge make
	node ./macos/collect-backend-deps.js --exe=$(PWD)/dist/gc-core --outdir=$(PWD)/dist

.PHONY: windows
windows: $(COMMON_DEPS)
	@cd backend && cp `stack path --local-install-root`/bin/gc-core.exe ../dist
	@$(NODE_BIN)/electron-forge make

dist:
	@mkdir -p $@

src/manifest.json:
	@echo '{"version": "$(VERSION)", "os": "$(OS)"}' > $@

.PHONY: frontend
frontend: \
	src/manifest.json
	@$(NODE_BIN)/parcel build --target electron --public-url . src/index.html

.PHONY: backend
backend:
	@cd backend && $(MAKE) result

.PHONY: macos-backend
macos-backend:
	@cd backend && $(MAKE) macos

.PHONY: windows-backend
windows-backend:
	@cd backend && make windows

node_modules: package.json
	@yarn

.PHONY: clean
clean:
	@echo "Cleaning"
	@rm -rf elm-stuff
	@rm -rf dist
	@rm -rf out

.PHONY: dev
dev: \
	node_modules \
	src/manifest.json
	@$(NODE_BIN)/parcel watch --target electron --public-url . src/index.html
