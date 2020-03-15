OS=$(shell uname -s)

EXE=
ifneq (,$(findstring MINGW,$(OS)))
	EXE=gc-core.exe
else
	EXE=gc-core
endif


.PHONY: build
build: out/$(EXE)

STACK_LOCAL_INSTALL_ROOT=$(shell stack path --local-install-root)
STACK_EXE=$(STACK_LOCAL_INSTALL_ROOT)/bin/$(EXE)

$(STACK_EXE): \
	$(shell find src) \
	$(shell find app) \
	package.yaml \
	stack.yaml
	@stack build

stack_exe: $(STACK_EXE)

ifeq ($(OS), Linux)
out/$(EXE): \
	stack2nix-output.nix \
	src
	@$$(nix-build --no-link -A buildScript --out-link result)
	@mkdir -p out
	@cp result/bin/gc-core $@
else
out/$(EXE): stack_exe
	@mkdir -p out
	@cp $(STACK_EXE) $@
endif

.PHONY: clean
clean:
	@echo "Cleaning"
	@rm -rf out
	@stack clean

stack2nix-output.nix: \
	package.yaml \
	stack.yaml
	@cat $$($$(nix-build --no-link -A stack2nix-script)) > $@

result: \
	stack2nix-output.nix \
	src
	@$$(nix-build --no-link -A buildScript --out-link $@)

.PHONY: dev
dev: 
	@ghcid --test ":main" --command=stack ghci

.PHONY: genElm
genElm:
	@stack run genElm
	@sed -i 's/D\.map T\.ProjectId D\.string/D.field "contents" <| D.map T.ProjectId D.string/' ../src/elm/IPC/Decoder.elm
	@sed -i 's/encodeProjectId = E\.string << T\.unProjectId/encodeProjectId pId = E.object [ ( "tag", E.string "ProjectId" ), ( "contents", E.string <| T.unProjectId pId ) ]/' ../src/elm/IPC/Encoder.elm
