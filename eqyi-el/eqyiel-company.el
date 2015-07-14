;;; eqyiel-company.el

(autoload 'global-company-mode "company" nil t)
(autoload 'company-dabbrev-code "company-dabbrev-code" nil t)
(autoload 'company-elisp "company-elisp" nil t)
(autoload 'company-gtags "company-gtags" nil t)
(autoload 'gtags-mode "gtags" nil t)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)

;; default backends
(setq company-backends
      '((company-abbrev
         company-yasnippet
         company-dabbrev
         company-files)))

;; emacs lisp
(defun eqyiel-company-elisp ()
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-elisp
          company-capf
          company-keywords
          company-dabbrev-code
          company-files))))
(add-hook 'emacs-lisp-mode-hook 'eqyiel-company-elisp)

(defun eqyiel-company-circe ()
  (set (make-local-variable 'company-backends)
       '((company-dabbrev))))
(add-hook 'circe-mode-hook 'eqyiel-company-circe)

(defun eqyiel-company-java ()
  (gtags-mode t)
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-gtags
          company-capf
          company-keywords
          company-dabbrev-code
          company-files))))
(add-hook 'java-mode-hook 'eqyiel-company-java)

(eval-after-load "company"
  '(progn
     (setq company-minimum-prefix-length 1
           company-idle-delay 0
           company-dabbrev-code-everywhere t)
     (diminish 'company-mode)))

(provide 'eqyiel-company)
