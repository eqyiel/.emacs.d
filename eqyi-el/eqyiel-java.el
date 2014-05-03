;;; eqyiel-java.el

(autoload 'eclim-mode "eclim" nil t)
(autoload 'start-eclimd "eclimd" nil t)
;; (autoload 'eclim-mode "ac-emacs-eclim-source")

(eval-after-load "eclim"
  '(setq eclim-java-documentation-root "/usr/share/doc/java7"
         eclim-auto-save nil)) ; annoying

(eval-after-load "eclimd"
  '(setq eclimd-default-workspace "~/doc/flinders/comp3751_s1_2014/code"))

(defun eqyiel-ac-java-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet
                            ac-source-filename
                            ac-source-emacs-eclim))))

(defun eqyiel-java-mode-hook ()
  (progn (eqyiel-ac-java-mode-setup)
         (eclim-mode)))

(define-key eclim-mode-map (kbd "C-c C-c") 'eclim-run-class)

(add-hook 'java-mode-hook 'eqyiel-java-mode-hook)

(provide 'eqyiel-java)
