NODE_BIN = $(PWD)/node_modules/.bin
OS=$(shell uname -s)
VERSION=$(shell cat version)

# no pun intended
out: \
	clean \
	package.json \
	node_modules \
	dist \
	backend \
	$(shell find icons) \
	$(shell find patch-repo) \
	electron.js \
	forge.config.js
	@$(NODE_BIN)/electron-forge make
	@touch -c out

.PHONY: clean
clean:
	@echo "Cleaning"
	@rm -rf elm-stuff
	@rm -rf dist
	@rm -rf out

node_modules: package.json
	@yarn
	@touch -c node_modules

src/manifest.json: version
	@echo '{"version": "$(VERSION)", "os": "$(OS)"}' > $@

package.json: version
	@jq '.version = "$(VERSION)"' package.json > package.json.tmp
	@mv package.json.tmp package.json

dist: \
	node_modules \
	$(shell find src) \
	src/manifest.json
	@$(NODE_BIN)/parcel build --target electron --public-url . src/index.html --out-dir $@
	@touch -c dist


.PHONY: backend
backend:
	@$(MAKE) -C backend 
	@mkdir -p dist
	@cp backend/out/gc-core* dist


.PHONY: dev
dev: \
	node_modules \
	src/manifest.json
	@$(NODE_BIN)/parcel watch --target electron --public-url . src/index.html

.PHONY: open
open:
	@electron electron.js
