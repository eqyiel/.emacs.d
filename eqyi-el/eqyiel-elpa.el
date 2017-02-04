;;; eqyiel-elpa.el

(require 'package)

(defvar eqyiel-package-list
  '(auth-password-store
    beacon
    calfw
    circe
    company
    company-emoji
    company-tern
    company-flx
    elnode
    elpy
    expand-region
    flx-ido
    flycheck
    gnus-desktop-notify
    ido-gnus
    ido-hacks
    ido-ubiquitous
    ido-vertical-mode
    idomenu
    japanese-holidays
    js2-mode
    js2-refactor
    nix-mode
    nodejs-repl
    notify
    pass
    popwin
    projectile
    rainbow-mode
    s
    skewer-mode
    solarized-theme
    sr-speedbar
    unfill
    web-mode
    zenburn-theme)
  "Packages to pull from `package-archives'.")

(eval-after-load "package"
  '(progn
     (setq package-user-dir "~/.emacs.d/site-lisp/elpa"
	   package-archives
	   '(("elpy" . "http://jorgenschaefer.github.io/packages/")
	     ("gnu" . "http://elpa.gnu.org/packages/")
	     ("marmalade" . "http://marmalade-repo.org/packages/")
	     ("melpa" . "http://melpa.org/packages/")
	     ("melpa-stable" . "http://stable.melpa.org/packages/")
	     ("org" . "http://orgmode.org/elpa/"))
	   package-pinned-packages
     '((elpy . "elpy")
	     (org-plus-contrib . "org")))
     (package-initialize)
     (unless package-archive-contents
       (package-refresh-contents))
     (dolist (package eqyiel-package-list)
       (unless (package-installed-p package)
         (package-install package)))))

(provide 'eqyiel-elpa)
