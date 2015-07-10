;;; eqyiel-c.el

(setq help-at-pt-timer-delay 0.1
      help-at-pt-display-when-idle t)

(defun eqyiel-cc-mode-hook ()
  (progn (setq tab-width 2
               c-basic-offset 2)
         (semantic-mode)))

(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)

(provide 'eqyiel-c)
