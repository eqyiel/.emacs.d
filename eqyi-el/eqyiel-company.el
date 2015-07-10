;;; eqyiel-company.el

(autoload 'global-company-mode "company" nil t)
(autoload 'company-dabbrev-code "company-dabbrev-code" nil t)
(autoload 'company-elisp "company-elisp" nil t)
(autoload 'company-gtags "company-gtags" nil t)
(autoload 'company-ispell "company-ispell" nil t)
(autoload 'gtags-mode "gtags" nil t)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)

;; default backends
(setq company-backends
      '((company-abbrev
         company-yasnippet
         company-dabbrev
         company-files
         company-ispell)))

;; emacs lisp
(defun eqyiel-company-elisp ()
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-elisp
          company-capf
          company-keywords
          company-dabbrev-code
          company-files
          company-ispell))))
(add-hook 'emacs-lisp-mode-hook 'eqyiel-company-elisp)

(defun eqyiel-company-circe ()
  (set (make-local-variable 'company-backends)
       '((company-dabbrev
          company-ispell))))
(add-hook 'circe-mode-hook 'eqyiel-company-circe)

(defun eqyiel-company-java ()
  (gtags-mode t)
  (set (make-local-variable 'company-backends)
       '((company-yasnippet
          company-gtags
          company-capf
          company-keywords
          company-dabbrev-code
          company-files
          ;; company-ispell
          ))))
(add-hook 'java-mode-hook 'eqyiel-company-java)

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)

(eval-after-load "company"
  '(progn
     (setq company-minimum-prefix-length 1
           company-idle-delay 0
           company-dabbrev-code-everywhere t)
     (diminish 'company-mode)))

(provide 'eqyiel-company)
