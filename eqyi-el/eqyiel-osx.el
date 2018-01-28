;;; eqyiel-osx.el

(require 'epg)
(setq epg-gpg-program "/usr/local/bin/gpg2")

;; This used to work.  See here for details:
;; https://lists.gnu.org/archive/html/emacs-devel/2016-06/msg00630.html
;;
;; Support for multi-color font is provided by the system, but is being actively
;; prevented by code in Emacs for political reasons.
;;
;; (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'prepend)
;; (set-fontset-font t nil (font-spec :family "Apple Symbols")  nil 'append)

(provide 'eqyiel-osx)
