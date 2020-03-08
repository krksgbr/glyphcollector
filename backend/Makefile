
clean:
	@echo "Cleaning"

stack2nix-output.nix: \
	package.yaml \
	stack.yaml
	@cat $$($$(nix-build --no-link -A stack2nix-script)) > $@

result: \
	stack2nix-output.nix \
	src
	@$$(nix-build --no-link -A buildScript --out-link $@)

dev: 
	@ghcid --test ":main"

genElm:
	@stack run genElm
	@sed -i 's/D\.map T\.ProjectId D\.string/D.field "contents" <| D.map T.ProjectId D.string/' ../src/elm/IPC/Decoder.elm
	@sed -i 's/encodeProjectId = E\.string << T\.unProjectId/encodeProjectId pId = E.object [ ( "tag", E.string "ProjectId" ), ( "contents", E.string <| T.unProjectId pId ) ]/' ../src/elm/IPC/Encoder.elm
