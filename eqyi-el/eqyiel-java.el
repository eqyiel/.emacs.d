;;; eqyiel-java.el

(autoload 'eclim-mode "eclim" nil t)
(autoload 'start-eclimd "eclimd" nil t)
;; (autoload 'eclim-mode "ac-emacs-eclim-source")

(eval-after-load "eclim"
  '(setq eclim-java-documentation-root "/usr/share/doc/java7"
         eclim-auto-save nil)) ; annoying

(eval-after-load "eclimd"
  '(progn
     (global-eclim-mode)
     (define-key eclim-mode-map (kbd "C-c C-c") 'eclim-run-class)
     (if (string-equal system-name "alcor.rkm.id.au")
         (setq eclimd-default-workspace "~/doc/flinders/comp3751_s1_2014/code")
       (setq eclimd-default-workspace "~/doc/comp3751/code"
             eclim-eclipse-dirs "/opt/eclipse"))))

(defun eqyiel-ac-java-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet
                            ac-source-filename
                            ac-source-emacs-eclim))))

(defun eqyiel-java-mode-hook ()
  (eqyiel-ac-java-mode-setup))

(add-hook 'java-mode-hook 'eqyiel-java-mode-hook)

(provide 'eqyiel-java)
