;;; eqyiel-misc.el

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
