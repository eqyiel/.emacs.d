;;; eqyiel-web.el

(autoload 'web-mode "web-mode")
(autoload 'rainbow-mode "rainbow-mode")
(autoload 'rainbow-turn-on "rainbow-mode")

(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(eval-after-load 'web-mode
  '(setq web-mode-markup-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-attr-indent-offset 2
         web-mode-sql-indent-offset 2))

(eval-after-load 'flycheck
  '(setq flycheck-html-tidy-executable "tidy"))

(eval-after-load 'flycheck
   '(flycheck-add-mode 'html-tidy 'web-mode))

(defun eqyiel-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-turn-on))
(add-hook 'css-mode-hook 'eqyiel-css-mode-hook)

(defun eqyiel-html-mode-hook ()
  (rainbow-turn-on))
(add-hook 'html-mode-hook 'eqyiel-html-mode-hook)

(provide 'eqyiel-web)
