;;; init.el

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(when (not (car (file-attributes "~/.cache/emacs")))
  (make-directory "~/.cache/emacs"))

(when (not (car (file-attributes "~/.local/share/emacs")))
  (make-directory "~/.local/share/emacs"))

(add-subdirs-to-load-path "~/.emacs.d")
(add-subdirs-to-load-path "~/.local/share/emacs")

(defmacro with-library (symbol &rest body)
  "See http://www.emacswiki.org/emacs/LoadingLispFiles"
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)
     (error (message (format "%s was not available." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

(with-library solarized
  (setq solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t))

;; (with-library zenburn-theme
;;   (load-theme 'zenburn t))

;; (require 'eqyiel-cedet)
(require 'eqyiel-elpa)
(require 'eqyiel-custom-junk)
(require 'eqyiel-defaults)
(require 'eqyiel-lib)
(require 'eqyiel-company)
(require 'eqyiel-org)
(require 'eqyiel-pass)
(require 'eqyiel-circe)
(require 'eqyiel-gnus)
(require 'eqyiel-ibuffer)
(require 'eqyiel-ido)
(require 'eqyiel-misc)
(require 'eqyiel-calfw)
(require 'eqyiel-python)
(require 'eqyiel-java)
(require 'eqyiel-tex)
(require 'eqyiel-javascript)
(require 'eqyiel-web)
(require 'eqyiel-android)
(require 'eqyiel-semantic)
(require 'eqyiel-scss)
(require 'eqyiel-flycheck)
