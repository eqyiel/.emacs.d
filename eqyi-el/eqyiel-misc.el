;;; eqyiel-misc.el

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

(setq key-chord-one-key-delay 0.01
      key-chord-two-keys-delay 0.01)

(autoload 'magit-status "magit" nil t)

(autoload 'buf-move-left "buffer-move" nil t)
(autoload 'buf-move-down "buffer-move" nil t)
(autoload 'buf-move-up "buffer-move" nil t)
(autoload 'buf-move-right "buffer-move" nil t)

;; Caps lock and Menu keys are bound to Hyper, except on OSX which apparently
;; can't into Hyper.  Use fake Hyper from Karabiner-elements instead, which is
;; really M-s-S-C.
;;
;; See:
;; http://www.tenshu.net/p/fake-hyper-key-for-osx.html
;; https://github.com/tekezo/Karabiner-Elements/pull/170

;; Fake hyper
(global-set-key (kbd "M-s-S-C-h") 'windmove-left)
(global-set-key (kbd "M-s-S-C-j") 'windmove-down)
(global-set-key (kbd "M-s-S-C-k") 'windmove-up)
(global-set-key (kbd "M-s-S-C-l") 'windmove-right)
(global-set-key (kbd "M-s-S-C-b") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-S-C-f") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-S-C-n") 'shrink-window)
(global-set-key (kbd "M-s-S-C-p") 'enlarge-window)

;; Real hyper
(global-set-key (kbd "H-h") 'windmove-left)
(global-set-key (kbd "H-j") 'windmove-down)
(global-set-key (kbd "H-k") 'windmove-up)
(global-set-key (kbd "H-l") 'windmove-right)
(global-set-key (kbd "H-b") 'shrink-window-horizontally)
(global-set-key (kbd "H-f") 'enlarge-window-horizontally)
(global-set-key (kbd "H-n") 'shrink-window)
(global-set-key (kbd "H-p") 'enlarge-window)
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
      flycheck-completion-system 'ido
      flycheck-error-list-minimum-level 'warning)

(global-set-key (kbd "C-c C-l") 'flycheck-list-errors)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'latex-mode-hook 'flycheck-mode)

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

(require 'projectile)
(projectile-global-mode)
(global-set-key (kbd "<f5>") 'projectile-compile-project)
(setq projectile-globally-ignored-directories
      (append '("dist" "node_modules") projectile-globally-ignored-directories))

(autoload 'run-skewer "skewer-mode" nil t)

(eval-after-load 'skewer-mode
  '(progn
     (add-hook 'js2-mode-hook 'skewer-mode)
     (add-hook 'css-mode-hook 'skewer-css-mode)
     (add-hook 'html-mode-hook 'skewer-html-mode)))

;;; `grep' doesn't know that GREP_OPTIONS is deprecated
;; (remove-hook 'grep-setup-hook (lambda () (setenv "GREP_OPTIONS" "")))

(use-package beacon
  :config (beacon-mode t))

(use-package sql-indent)

(eval-after-load 'flycheck
  '(flycheck-define-checker swiftlint
    "Flycheck plugin for Swiftlint"
    :command ("swiftlint")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": "
            "error: " (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": "
              "warning: " (message) line-end))
    :modes swift-mode))

(use-package swift-mode
  :config (setq swift-indent-offset 2
                         flycheck-swift-sdk-path
                         (concat "/Applications/Xcode.app/Contents/Developer"
                                 "/Platforms/iPhoneOS.platform/Developer/SDKs/"
                                 "iPhoneOS9.3.sdk"))
  :init (progn '((add-to-list 'flycheck-checkers 'swift)
                 (add-to-list 'flycheck-checkers 'swiftlint)
                 (flycheck-add-next-checker 'swiftlint '(t . swift))))
  :ensure flycheck)

(use-package yaml-mode
  :ensure t)

(provide 'eqyiel-misc)
