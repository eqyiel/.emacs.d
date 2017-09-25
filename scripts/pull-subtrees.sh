#!/usr/bin/env bash

git subtree pull --prefix site-lisp/nixos/nix-mode nixos/nix-mode master --squash
git subtree pull --prefix site-lisp/travisbhartwell/nix-emacs travisbhartwell/nix-emacs master --squash
