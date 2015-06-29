;;; eqyiel-web.el

(autoload 'rainbow-mode "rainbow-mode" nil t)

(defun eqyiel-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-mode)
  (rainbow-turn-on))

(add-hook 'css-mode-hook 'eqyiel-css-mode-hook)

(defun eqyiel-html-mode-hook ()
  (rainbow-mode)
  (rainbow-turn-on))

(add-hook 'html-mode-hook 'eqyiel-html-mode-hook)

(provide 'eqyiel-web)
