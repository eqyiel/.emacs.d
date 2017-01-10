;;; eqyiel-cc.el

(use-package c++-mode
  :config (c-set-offset 'arglist-cont-nonempty '+)
  :bind (:map c++-mode-map ("C-c C-l" . flycheck-list-errors)))

(use-package flycheck
  :config (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
  :ensure t)

(use-package flycheck-google-cpplint
  :init (setq flycheck-c/c++-googlelint-executable
              (executable-find "cpplint"))
  :config (progn ;; what the flycheck
            ;; I want to run all checkers, always, so use t instead of a minimum
            ;; warning level.  Keep an eye on this for future developments:
            ;; https://github.com/flycheck/flycheck/issues/836
            (flycheck-add-next-checker
             'c/c++-gcc
             '(t . c/c++-cppcheck))
            (flycheck-add-next-checker
             'c/c++-cppcheck
             '(t . c/c++-googlelint)))
  :ensure flycheck
  :ensure google-c-style)

(use-package google-c-style
  :init (add-hook 'c-mode-common-hook 'google-set-c-style)
  :ensure t)

(provide 'eqyiel-cc)
