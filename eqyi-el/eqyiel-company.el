;;; eqyiel-company.el

(autoload 'global-company-mode "company" nil t)
(autoload 'company-complete "company" nil t)
(autoload 'company-dabbrev-code "company-dabbrev-code" nil t)
(autoload 'company-elisp "company-elisp" nil t)
(autoload 'company-emoji "company-emoji" nil t)
(autoload 'company-gtags "company-gtags" nil t)
(autoload 'gtags-mode "gtags" nil t)
(autoload 'company-flx-mode "company-flx")

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)

;; default backends
(setq company-backends
      '((company-files
         company-yasnippet
         company-emoji)))

;; emacs lisp
(defun eqyiel-company-elisp ()
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-elisp
          ;; company-capf
          ;; copmany-semantic
          company-keywords
          company-dabbrev-code
          company-files))))
(add-hook 'emacs-lisp-mode-hook 'eqyiel-company-elisp)

;; (defun eqyiel-company-circe ()
;;   (set (make-local-variable 'company-backends)
;;        '((company-dabbrev))))

(add-hook 'circe-channel-mode-hook (lambda () (company-mode -1)))

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

(defun eqyiel-company-processing ()
  (gtags-mode t)
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-keywords
          company-dabbrev-code
          company-files))))
(add-hook 'processing-mode-hook 'eqyiel-company-processing)

(defun eqyiel-company-markdown ()
  (add-hook 'markdown-mode-hook 'company-mode)
  (add-hook 'markdown-mode-hook 'company-emoji-init))

(defun eqyiel-company-shell ()
  (set (make-local-variable 'company-backends)
       '((company-capf))))
(add-hook 'shell-mode-hook 'eqyiel-company-shell)

;; (eval-after-load "shell"
;;   (define-key shell-mode-map (kbd "TAB") 'company-manual-begin))

;; doesn't work :(
;; (with-eval-after-load 'company
;;   (company-flx-mode t))

(eval-after-load "company"
  '(progn
     (setq company-minimum-prefix-length 1
           company-idle-delay 0
           company-dabbrev-code-everywhere t)
     (diminish 'company-mode)))

(provide 'eqyiel-company)
