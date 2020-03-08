
NODE_BIN = node_modules/.bin

all: \
	clean \
	backend \
	node_modules
	@mkdir -p dist
	@$(NODE_BIN)/parcel build --public-url . src/index.html
	@cp backend/result/bin/gc-core dist
	@$(NODE_BIN)/electron-forge make

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
