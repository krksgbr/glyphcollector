NODE_BIN = $(PWD)/node_modules/.bin
OS=$(shell uname -s)

all:

ifeq ($(OS), Darwin)
all: macos
else
all: linux
endif

ifeq (, $(findstring $(OS), MINGW))
all: windows
endif


linux: \
	clean \
	backend \
	node_modules
	@mkdir -p dist
	@$(NODE_BIN)/parcel build --public-url . src/index.html
	@cp backend/result/bin/gc-core dist
	@$(NODE_BIN)/electron-forge make

macos: \
	clean \
	macos-backend \
	node_modules
	@mkdir -p dist
	@$(NODE_BIN)/parcel build --public-url . src/index.html
	@cd backend && cp `stack path --local-install-root`/bin/gc-core ../dist
	@$(NODE_BIN)/electron-forge make
	node ./macos/collect-backend-deps.js --exe=$(PWD)/dist/gc-core --outdir=$(PWD)/dist

windows: \
	clean \
	windows-backend \
	node_modules
	@mkdir -p dist
	@$(NODE_BIN)/parcel build --public-url . src/index.html
	@cd backend && cp `stack path --local-install-root`/bin/gc-core.exe ../dist
	@$(NODE_BIN)/electron-forge make

e:
	@echo $(NODE_BIN)

macos-backend:    
	@cd backend && $(MAKE) macos

windows-backend:
	@cd backend && make windows
	
node_modules:
	@yarn

clean:
	@echo "Cleaning"
	@rm -rf elm-stuff
	@rm -rf dist
	@rm -rf out

backend:
	@cd backend && $(MAKE) result

dev: \
	node_modules
	@yarn startParcel
