(require 'eclim)
(global-eclim-mode)
(require 'eclimd)
(require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

(setq eclim-java-documentation-root "/usr/share/doc/java7")

;; doesn't appear to work
(setq eclim-default-workspace "~/doc/flinders/comp3751_s1_2014/code")

;; why does this make emacs lock up?
(setq eclim-auto-save nil)              ; annoying!

(defun ac-java-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet
                            ac-source-filename
                            ac-source-emacs-eclim
                            ;; ac-source-gtags
                            ;; ac-source-semantic-raw
                            ac-source-semantic))))

(defun my-java-mode-hook ()
  (progn (ac-java-mode-setup)
         (setq tab-width 2
               c-basic-offset 2)))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(provide 'eqyiel-java)
