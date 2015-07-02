;;; eqyiel-cedet.el

;; Hey, you!  If you're cloning this for the first time make sure to
;; $ touch .nosearch
;; in site-lisp/cedet so that it won't be read by add-subdirs-to-load-path.

;; Load the development version rather than emacs builtin.
(load-file "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el")

(eval-after-load "semantic"
  '(progn
     (unless (file-exists-p "~/.cache/emacs/semanticdb")
       (make-directory "~/.cache/emacs/semanticdb" t)) ; also create parents
     (setq semanticdb-default-save-directory "~/.cache/emacs/semanticdb")))

(eval-after-load "cedet-java"
  ;; default fails to find openjdk on my system
  '(setq cedet-java-version-regexp "version \"\\([0-9._]+\\)\""))

(provide 'eqyiel-cedet)
