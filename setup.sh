#!/bin/sh
cd ~/.emacs.d/
git submodule init
git submodule update
emacs -q -l ~/.emacs.d/eqyi-el/eqyiel-elpa.el --batch
