NODE_BIN = $(PWD)/node_modules/.bin
OS=$(shell uname -s)
VERSION=0.2.1

out: \
	clean \
	package_version \
	node_modules \
	dist \
	backend \
	$(shell find icons) \
	$(shell find patch-repo) \
	electron.js \
	forge.config.js
	@$(NODE_BIN)/electron-forge make

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

.PHONY: package_version
package_version: version
	@jq '.version = "$(VERSION)"' package.json > package.json.tmp
	@mv package.json.tmp package.json

dist: \
	node_modules \
	$(shell find src) \
	src/manifest.json
	@$(NODE_BIN)/parcel build --target electron --public-url . src/index.html --out-dir $@ \

test: $(COMMON_DEPS)


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
