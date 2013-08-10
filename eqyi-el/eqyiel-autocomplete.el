;;; eqyiel-autocomplete.el

(require 'auto-complete)
(require 'popup)
(require 'fuzzy)
(require 'ert)
(require 'diminish)
(require 'auto-complete-config)

;; this is kind of too much stuff
(setq-default ac-sources '(ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
                           ac-source-filename))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features
                             ac-source-functions
                             ac-source-variables
                             ac-source-symbols)
                           ac-sources)))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

(defun ac-css-mode-setup ()
  (setq ac-sources (append '(ac-source-css-property) ac-sources)))

(add-hook 'css-mode-hook 'ac-css-mode-setup)

(global-auto-complete-mode t)
(ac-flyspell-workaround)

(setq ac-auto-show-menu 0.1
      ac-quick-help-delay 0.2
      ac-use-menu-map t
      ac-auto-start t
      ac-comphist-file "~/.cache/emacs/ac-comphist.dat")

(diminish 'auto-complete-mode)


(provide 'eqyiel-autocomplete)
