;;; eqyiel-pass.el

(require 'password-store)
(require 'auth-password-store)

(eval-after-load "auth-source"
  ;; Don't try to read from ~/.authinfo{,.gpg} or ~/.netrc
  '(setq auth-sources nil))

(auth-pass-enable)

(provide 'eqyiel-pass)
