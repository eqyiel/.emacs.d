;;; init.el

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/.emacs.d")
(add-subdirs-to-load-path "~/.local/share/emacs")

(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes/zenburn")
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes/solarized")

(load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

(require 'eqyiel-defaults)
(require 'eqyiel-lib)
(require 'eqyiel-abbrev)
(require 'eqyiel-autocomplete)
(require 'eqyiel-org)
(require 'eqyiel-circe)
;; (require 'eqyiel-desktop)
;; (require 'eqyiel-gnuplot)
(require 'eqyiel-gnus)
(require 'eqyiel-guide-key)
(require 'eqyiel-ibuffer)
(require 'eqyiel-ido)
(require 'eqyiel-misc)
(require 'eqyiel-calfw)
;; ;; (require 'eqyiel-slime)
;; ;; (require 'eqyiel-python)
;; ;; (require 'eqyiel-haskell)
;; (require 'eqyiel-c)
(require 'eqyiel-java)
;; (require 'eqyiel-tex)
;; ;; (require 'eqyiel-emms)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

(setq display-time-world-list '(("Asia/Tokyo" "Tokyo")
                                ("Australia/Adelaide" "Adelaide")
                                ("Australia/Brisbane" "Brisbane")
                                ("Australia/Canberra" "Canberra")
                                ("Australia/Darwin" "Darwin")
                                ("Australia/Hobart" "Hobart")
                                ("Australia/Melbourne" "Melbourne")
                                ("Australia/Perth" "Perth")
                                ("Australia/Sydney" "Sydney")))
