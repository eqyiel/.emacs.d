;;; eqyiel-flycheck.el

(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)

(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(handlebars html-tidy javascript-jshint javascript-jscs)))

(setq flycheck-eslintrc "~/.eslintrc"
      flycheck-jscsrc "~/.jscsrc"
      flycheck-jshintrc "~/.jshintrc"
      flycheck-javascript-eslint-executable "eslint"
      flycheck-javascript-jscs-executable "jscs"
      flycheck-javascript-jshint-executable "jshint"
      flycheck-scss-lint-executable "scss-lint")

;; (flycheck-add-next-checker 'javascript-jshint '(error . javascript-jscs))

;; (cl-rotatef
;;  (nth (-elem-index 'javascript-jshint flycheck-checkers) flycheck-checkers)
;;  (nth (-elem-index 'javascript-eslint flycheck-checkers) flycheck-checkers))

(provide 'eqyiel-flycheck)
