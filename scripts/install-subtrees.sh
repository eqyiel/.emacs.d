#!/usr/bin/env bash

git subtree add --prefix site-lisp/nixos/nix-mode nixos/nix-mode master --squash
git subtree add --prefix site-lisp/travisbhartwell/nix-emacs travisbhartwell/nix-emacs master --squash
