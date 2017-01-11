;;; eqyiel-scss.el

(require 'use-package)

(use-package scss-mode
  :config (add-hook
           'scss-mode-hook
           (lambda () ;; use // instead of /* */
             (set (make-local-variable 'comment-start) "//")
             (set (make-local-variable 'comment-end) "")
             (set (make-local-variable 'comment-continue) "//")))
  :ensure t)

 (provide 'eqyiel-scss)
