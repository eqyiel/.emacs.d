;;; eqyiel-misc.el

;; first pull in libraries we use all the time

;; diminish --------------------------------------------------------------------

(require 'diminish)

;; smartparens -----------------------------------------------------------------

(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(sp-use-paredit-bindings)
(diminish 'smartparens-mode)

;; don't do this: "\"\""
(setq sp-autoescape-string-quote nil)
(setq sp-autoskip-closing-pair 'always)

;; yasnippet -------------------------------------------------------------------

(require 'yasnippet)

(eval-after-load "yasnippet"
  '(progn
     (setq yas-snippet-dirs
           '("~/.emacs.d/eqyi-el/snippets")
           yas-prompt-functions '(yas-ido-prompt))
     (diminish 'yas-minor-mode)))

(yas-global-mode t)
(global-set-key (kbd "C-c TAB") 'yas-expand)

;; key-chord -------------------------------------------------------------------

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'er/expand-region)

;; everything beyond here is loaded on-demand

;; magit -----------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)

;; switch between windows more easily ------------------------------------------

(global-set-key (kbd "H-h") 'windmove-left)
(global-set-key (kbd "H-j") 'windmove-down)
(global-set-key (kbd "H-k") 'windmove-up)
(global-set-key (kbd "H-l") 'windmove-right)

(global-set-key (kbd "H-b") 'shrink-window-horizontally)
(global-set-key (kbd "H-f") 'enlarge-window-horizontally)
(global-set-key (kbd "H-n") 'shrink-window)
(global-set-key (kbd "H-p") 'enlarge-window)

(autoload 'buf-move-left "buffer-move" nil t)
(autoload 'buf-move-down "buffer-move" nil t)
(autoload 'buf-move-up "buffer-move" nil t)
(autoload 'buf-move-right "buffer-move" nil t)

(global-set-key (kbd "M-H-h") 'buf-move-left)
(global-set-key (kbd "M-H-j") 'buf-move-down)
(global-set-key (kbd "M-H-k") 'buf-move-up)
(global-set-key (kbd "M-H-l") 'buf-move-right)

(autoload 'tiling-cycle "tiling" nil t)

(global-set-key (kbd "H-SPC") 'tiling-cycle)

;; smex ------------------------------------------------------------------------

(eval-after-load "smex"
  '(setq smex-save-file "~/.cache/emacs/smex-items"))

(autoload 'smex "smex" nil t)
(autoload 'smex-major-mode-commands "smex" nil t)
(autoload 'execute-extended-command "smex" nil t)
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; multiple-cursors ------------------------------------------------------------

(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(autoload 'mc-mark-previous-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-all-like-this "multiple-cursors" nil t)
(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/edit-ends-of-lines "multiple-cursors" nil t)
(autoload 'mc/edit-beginnings-of-lines "multiple-cursors" nil t)
(autoload 'set-rectangular-region-anchor "multiple-cursors" nil t)

(eval-after-load "multiple-cursors"
  '(setq mc/list-file "~/.cache/emacs/mc-lists.el"))

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

;; expand-region ---------------------------------------------------------------

(autoload 'er/expand-region "expand-region" nil t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; column-marker ---------------------------------------------------------------

(autoload 'column-marker-1 "column-marker" nil t)
(global-set-key (kbd "C-c m") 'column-marker-1)

;; highlight-indentation -------------------------------------------------------

(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)

(eval-after-load "highlight-indentation"
  '(diminish 'highlight-indentation-mode))

;; fill-column-indicator -------------------------------------------------------

;; https://github.com/alpaker/Fill-Column-Indicator
;; (require 'fill-column-indicator)
(autoload 'turn-on-fci-mode "fill-column-indicator" nil t)
(autoload 'fci-mode "fill-column-indicator" nil t)
(add-hook 'prog-mode-hook 'turn-on-fci-mode)

;; https://github.com/alpaker/Fill-Column-Indicator/issues/21
(defvar sanityinc/fci-mode-suppressed nil)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  (when fci-mode
    (turn-off-fci-mode)))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; git-gutter ------------------------------------------------------------------

;; this make start up pretty slow when enabled globally
;; maybe it would be better hooked into projectile or something?
(autoload 'git-gutter-mode "git-gutter" nil t)
(autoload 'global-git-gutter-mode "git-gutter" nil t)
;; (global-git-gutter-mode t)
(eval-after-load "git-gutter"
  '(diminish 'git-gutter-mode))

;; pkgbuild-mode ---------------------------------------------------------------

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                              auto-mode-alist))

;; nethack ---------------------------------------------------------------------

(autoload 'nethack "nethack" "Play Nethack." t)
(setq nethack-program "/usr/bin/nethack"
      nethack-use-tiles t)

;; legalese --------------------------------------------------------------------

(autoload 'legalese "legalese" nil t)

(provide 'eqyiel-misc)
