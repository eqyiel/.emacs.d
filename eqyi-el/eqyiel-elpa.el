;;; eqyiel-elpa.el

(require 'package)

(defvar eqyiel-package-list
  '(auth-password-store
    buffer-move
    calfw
    circe
    column-enforce-mode
    column-marker
    company
    diminish
    dtrt-indent
    elnode
    elpy
    emacs-eclim
    expand-region
    fill-column-indicator
    flx-ido
    flycheck
    git-gutter
    guide-key
    helm
    helm-pydoc
    helm-swoop
    idle-highlight-mode
    ido-ubiquitous
    ido-vertical-mode
    idomenu
    japanese-holidays
    js2-mode
    js2-refactor
    key-chord
    legalese
    magit
    markdown-mode
    multiple-cursors
    notify
    org-caldav
    org-plus-contrib
    pass
    pkgbuild-mode
    popwin
    rainbow-mode
    s
    smartparens
    smex
    solarized-theme
    unfill
    visual-fill-column
    web-mode
    ws-butler
    yasnippet
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
	   '((circe . "marmalade")
	     (elpy . "elpy")
	     (org-plus-contrib . "org")))
     (package-initialize)
     (unless package-archive-contents
       (package-refresh-contents))
     (dolist (package eqyiel-package-list)
       (unless (package-installed-p package)
	 (package-install package)))))

(provide 'eqyiel-elpa)
