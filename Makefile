.PHONY: run-dev

run-dev:
	`nix-build -A run-example -j auto`
