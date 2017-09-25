#!/usr/bin/env bash

set -euo pipefail

git subtree add --prefix site-lisp/nixos/nix-mode nixos/nix-mode master --squash
