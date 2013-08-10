;;; eqyiel-misc.el

;; pull in libraries that aren't part of emacs, and don't have complex enough
;; configuration to warrant their own file

(require 'magit)
(require 'diminish)
(require 'tiling)
(require 'buffer-move)
(global-set-key (kbd "H-SPC") 'tiling-cycle)
(global-set-key (kbd "M-H-k") 'buf-move-up)
(global-set-key (kbd "M-H-j") 'buf-move-down)
(global-set-key (kbd "M-H-h") 'buf-move-left)
(global-set-key (kbd "M-H-l") 'buf-move-right)

(setq smex-save-file "~/.cache/emacs/smex-items")

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'yasnippet)
(setq yas-snippet-dirs (quote ("~/.emacs.d/config/snippets"
                               "~/.emacs.d/site-lisp/yasnippet/snippets")))
(yas-global-mode t)
(global-set-key (kbd "C-c TAB") 'yas-expand)
(setq yas-prompt-functions '(yas-ido-prompt))
(diminish 'yas-minor-mode)

(require 'multiple-cursors)
(setq mc/list-file "~/.cache/emacs/mc-lists.el")
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;; Rectangular region mode
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'column-marker)
(global-set-key (kbd "C-c m") 'column-marker-1)

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(sp-use-paredit-bindings)
(diminish 'smartparens-mode)

(require 'highlight-indentation)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(diminish 'highlight-indentation-mode)

;; https://github.com/alpaker/Fill-Column-Indicator
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook 'turn-on-fci-mode)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

(autoload 'nethack "nethack" "Play Nethack." t)
(setq nethack-program "/usr/bin/nethack"
      nethack-use-tiles t)

(provide 'eqyiel-misc)
