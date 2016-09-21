;;; eqyiel-scss.el

(eval-after-load 'scss-mode
  (setq scss-sass-command "~/.gem/ruby/2.3.0/bin/sass"

  ))

;; use // instead of /* */
(add-hook 'scss-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-start) "//")
            (set (make-local-variable 'comment-end) "")
            (set (make-local-variable 'comment-continue) "//")))



(provide 'eqyiel-scss)
