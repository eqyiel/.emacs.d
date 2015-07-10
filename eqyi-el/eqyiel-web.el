;;; eqyiel-web.el

(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (defun eqyiel-web-mode-hook ()
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         ;; Use server-side comment instead of html/css/js
;;         web-mode-comment-style 2))

;; (add-hook 'web-mode-hook 'eqyiel-web-mode-hook)

(autoload 'rainbow-mode "rainbow-mode" nil t)

(defun eqyiel-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-mode))

(add-hook 'css-mode-hook 'eqyiel-css-mode-hook)

(defun eqyiel-html-mode-hook ()
  (rainbow-mode))

(add-hook 'html-mode-hook 'eqyiel-html-mode-hook)

(provide 'eqyiel-web)
