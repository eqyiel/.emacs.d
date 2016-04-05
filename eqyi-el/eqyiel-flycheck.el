;;; eqyiel-flycheck.el

(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)

(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint javascript-jscs)))

(setq flycheck-eslintrc "~/.eslintrc"
      flycheck-javascript-eslint-executable "eslint")

(provide 'eqyiel-flycheck)
