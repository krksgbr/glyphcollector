
clean:
	@echo "Cleaning"

stack2nix-output.nix: \
	package.yaml \
	stack.yaml
	@cat $$($$(nix-build --no-link -A stack2nix-script)) > $@

result: \
	stack2nix-output.nix
	@$$(nix-build --no-link -A buildScript --out-link $@)
