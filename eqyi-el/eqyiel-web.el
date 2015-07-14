;;; eqyiel-web.el

(autoload 'web-mode "web-mode")
(autoload 'rainbow-mode "rainbow-mode")
(autoload 'rainbow-turn-on "rainbow-mode")

(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun eqyiel-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-turn-on))
(add-hook 'css-mode-hook 'eqyiel-css-mode-hook)

(defun eqyiel-html-mode-hook ()
  (rainbow-turn-on))
(add-hook 'html-mode-hook 'eqyiel-html-mode-hook)

(provide 'eqyiel-web)
