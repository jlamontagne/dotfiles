.DEFAULT: stow
.PHONY: stow
PKGS = $(shell find . -depth 1 -type d -not -name '.git' -printf '%f ')
stow:
	stow -v $(PKGS)
