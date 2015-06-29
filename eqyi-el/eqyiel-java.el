;;; eqyiel-java.el

(autoload 'eclim-mode "eclim" nil t)
(autoload 'start-eclimd "eclimd" nil t)
(autoload 'company-emacs-eclim "company-emacs-eclim" nil t)
;; (autoload 'eclim-mode "ac-emacs-eclim-source")

(eval-after-load "eclim"
  '(setq eclim-java-documentation-root "/usr/share/doc/java7"
         eclim-auto-save nil)) ; annoying

(eval-after-load "eclim"
  '(setq company-eclim-auto-save nil
         company-eclim-executable
         "eclim"))

(eval-after-load "eclimd"
  '(progn
     (global-eclim-mode)
     (define-key eclim-mode-map (kbd "C-c C-c") 'eclim-run-class)
     (define-key eclim-mode-map (kbd "<f5>") 'eclim-project-build)
     (setq eclimd-default-workspace "~/doc/flinders/comp3712_s1_2015/")))

(defun eqyiel-java-mode-hook ()
  (company-emacs-eclim-setup))

;; (defun eqyiel-java-mode-hook ()
;;   (set (make-local-variable 'company-backends) '((company-eclim
;;                                                   company-emacs-eclim
;;                                                   company-yasnippet))))

(add-hook 'java-mode-hook 'eqyiel-java-mode-hook)

(provide 'eqyiel-java)
