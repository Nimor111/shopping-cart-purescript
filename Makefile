build:
	nix-build default.nix

format:
	nix-shell shell.nix --run 'purty --write src&& purte --write test'

shell:
	nix-shell shell.nix

.PHONY: build format shell
