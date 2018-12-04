.DEFAULT: stow
.PHONY: stow
PKGS = $(shell find -mindepth 1 -maxdepth 1 -type d -not -name '.git' -printf '%f ')
stow:
	stow -v $(PKGS)
