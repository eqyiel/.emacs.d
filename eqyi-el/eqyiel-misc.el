;;; eqyiel-misc.el


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

(use-package php-mode :ensure t)

(provide 'eqyiel-misc)
