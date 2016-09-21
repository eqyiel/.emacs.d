;;; eqyiel-osx.el

(require 'epg)
(setq epg-gpg-program "/usr/local/bin/gpg2")

(set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                  frame 'prepend)
(set-fontset-font t nil (font-spec :family "Apple Symbols")
                  frame 'append)

(provide 'eqyiel-osx)
