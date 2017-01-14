;;; eqyiel-web.el

(autoload 'web-mode "web-mode")
(autoload 'rainbow-mode "rainbow-mode")
(autoload 'rainbow-turn-on "rainbow-mode")

(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(eval-after-load 'web-mode
  '(progn
     (setq web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-attr-indent-offset 2
           web-mode-sql-indent-offset 2
           web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))
     (add-hook 'web-mode-hook
               '(lambda ()
                  (when (equal web-mode-content-type "jsx")
                    (flycheck-select-checker 'javascript-eslint))))))

(eval-after-load 'flycheck
  '(setq flycheck-html-tidy-executable "tidy"))

(eval-after-load 'flycheck
   '(progn
      (flycheck-add-mode 'html-tidy 'web-mode)
      (flycheck-add-mode 'javascript-eslint 'web-mode)))

(defun eqyiel-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-turn-on))
(add-hook 'css-mode-hook 'eqyiel-css-mode-hook)

(defun eqyiel-html-mode-hook ()
  (rainbow-turn-on))
(add-hook 'html-mode-hook 'eqyiel-html-mode-hook)

(defun eqyiel-toggle-web-mode-to-js2-mode ()
  (interactive)
  (if (string-equal web-mode-content-type "jsx")
      (js2-jsx-mode)
    (js2-mode)))

(use-package web-mode
  :config (progn
            (define-key web-mode-map (kbd "C-M-s-\"")
              'eqyiel-toggle-web-mode-to-js2-mode)
            (define-key web-mode-map (kbd "H-'")
              'eqyiel-toggle-web-mode-to-js2-mode)
            (define-key web-mode-map (kbd "M-j")
              'newline-and-indent)
            (setq web-mode-content-types-alist
                  '(("jsx" . "\\.js[x]?\\'"))))
  :ensure t)

(provide 'eqyiel-web)

