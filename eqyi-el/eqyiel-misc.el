;;; eqyiel-misc.el

(require 'diminish)

(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(sp-use-paredit-bindings)
(diminish 'smartparens-mode)

;; Don't do this: "\"\""
(setq sp-autoescape-string-quote nil)
(setq sp-autoskip-closing-pair 'always)

(require 'yasnippet)

(eval-after-load "yasnippet"
  '(progn
     (setq yas-snippet-dirs '("~/.emacs.d/eqyi-el/snippets"
                             yas-installed-snippets-dir)
           yas-prompt-functions '(yas-ido-prompt))
     (diminish 'yas-minor-mode)))

(yas-global-mode t)
(global-set-key (kbd "C-c TAB") 'yas-expand)

(autoload 'er/expand-region "expand-region" nil t)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global "jk" 'er/expand-region)
(key-chord-define-global "jo" 'other-window)

(autoload 'magit-status "magit" nil t)

;; Caps lock and Menu keys are bound to Hyper.
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

(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-previous-like-this "multiple-cursors" nil t)
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

(autoload 'column-marker-1 "column-marker" nil t)
(global-set-key (kbd "C-c m") 'column-marker-1)

(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(eval-after-load "highlight-indentation"
  '(diminish 'highlight-indentation-mode))

(autoload 'idle-highlight-mode "idle-highlight-mode" nil t)

(autoload 'column-enforce-mode "column-enforce-mode" nil t)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(eval-after-load "column-enforce-mode" '(diminish 'column-enforce-mode))

(autoload 'turn-on-fci-mode "fill-column-indicator" nil t)
(autoload 'fci-mode "fill-column-indicator" nil t)
(add-hook 'prog-mode-hook 'turn-on-fci-mode)

;; https://github.com/alpaker/Fill-Column-Indicator/issues/21
(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
      (turn-off-fci-mode))))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(autoload 'git-gutter-mode "git-gutter" nil t)
(autoload 'global-git-gutter-mode "git-gutter" nil t)
;; This makes startup really slow.  Might be better hooked into projectile or
;; something.
;; (global-git-gutter-mode t)
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

(autoload 'legalese "legalese" nil t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'ws-butler)
(ws-butler-global-mode)
(diminish 'ws-butler-mode)

(require 'dtrt-indent)
(dtrt-indent-mode)

(require 'flycheck)
(setq flycheck-gcc-pedantic t
      flycheck-display-errors-delay 0.1
      flycheck-completion-system 'ido)

(add-hook 'prog-mode-hook 'flycheck-mode)

(autoload 'internodeum/usage-summary "internodeum" nil t)

(defadvice internodeum/usage-summary (before eqyiel-internodeum-setup activate)
  (setq internodeum/credentials
        (internodeum/make-creds
         :username "eqyiel"
         :password (password-store-get "internode"))))

(defadvice internodeum/usage-summary (after eqyiel-internodeum-clear activate)
  (setf (internodeum/creds-username internodeum/credentials) nil)
  (setf (internodeum/creds-password internodeum/credentials) nil)
  (setq internodeum/credentials nil))

;; Together, these make a nice replacement for longlines-mode.
(autoload 'turn-on-visual-line-mode "visual-line-mode" nil t)
(autoload 'turn-on-visual-fill-column-mode "visual-fill-column" nil t)

(provide 'eqyiel-misc)
