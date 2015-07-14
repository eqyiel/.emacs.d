;;; eqyiel-cedet.el

;; Hey, you!  If you're cloning this for the first time make sure to
;; $ touch .nosearch
;; in site-lisp/cedet so that it won't be read by
;; `add-subdirs-to-load-path'. Also, load this early in init.el otherwise the
;; builtin CEDET will already be loaded.

;; Load the development version rather than emacs builtin.
(load-file "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el")

(eval-after-load "semantic"
  '(setq semanticdb-default-save-directory "~/.cache/emacs/semanticdb"))

(eval-after-load "cedet-java"
  ;; Default fails to find OpenJDK on my system
  '(setq cedet-java-version-regexp "version \"\\([0-9._]+\\)\""))

(provide 'eqyiel-cedet)
