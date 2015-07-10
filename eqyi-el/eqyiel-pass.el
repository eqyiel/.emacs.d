;;; eqyiel-pass.el
;;
;; `auth-password-store' is not very well documented so I'll keep a note of my
;; experiences here.  Note that these entries in the password store don't have
;; to be in the root folder. Mine are all in a directory auth-sources.
;;
;; For use with `tramp':
;; You need a file with the same user@host format that you would supply to
;; find-file.  Additionally, the port entry should match the protocol being used
;; by `tramp'.
;;
;; For instance: "/sudo:root@localhost:/etc/file-owned-by-root".
;;
;; In this case the pass entry will have the form:
;;
;; $ pass edit auth-sources/root@localhost
;; <password>
;; host: localhost
;; user: root
;; port: sudo
;;
;; For use with `gnus' and a local Dovecot server:
;; You need a file with the same user@host format you would supply to
;; gnus-select-method.  For instance:
;;
;; (eval-after-load "gnus"
;;   '(setq gnus-select-method
;;          ;; there should be a pass entry eqyiel@localhost
;;          '(nnimap "eqyiel@localhost"
;;                   (nnimap-address "localhost")
;;                   (nnimap-stream network))))
;;
;; In this case the pass entry will have the form:
;;
;; $ pass edit auth-sources/eqyiel@localhost
;; <password>
;; host: localhost
;; user: eqyiel
;; port: imap
;;
;; Be sure to check what port your server is listening on.  Mine is 143, but you
;; need to use "port: imaps" if yours is running on 993.
;;
;; For use with `gnus' and a remote IMAP server:
;; You need a file with the format user@imap-server.  Be sure to use the same
;; imap-server as the first argument to `nnimap'. For instance:
;;
;; (eval-after-load "gnus"
;;   '(setq gnus-select-method
;;          '(nnimap "rkm.id.au"
;;                   (nnimap-address "rkm.id.au")
;;                   (nnimap-server-port 993)
;;                   (nnimap-stream ssl))
;;          gnus-secondary-select-methods
;;          '((nnimap "imap.gmail.com"
;;                    (nnimap-address "imap.gmail.com")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl))
;;            (nnimap "outlook.office365.com"
;;                    (nnimap-address "outlook.office365.com")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl)))))
;;
;; In this case the pass entry will look like this:
;;
;; $ pass edit auth-sources/eqyiel@imap.gmail.com
;; <password>
;; host: imap.gmail.com
;; user: eqyiel@gmail.com
;;
;; For use with `smtpmail':
;; You need a file with the format user@smtp-server.  Note that the SMTP
;; hostname may be different to the IMAP hostname.  For instance, Microsoft
;; Exchange uses smtp.office365.com and outlook.office365.com (IMAP), and Google
;; uses smtp.gmail.com and imap.gmail.com.  Fortunately both outlook.office365
;; and imap.google.com listen on port 587 so in this case you don't need to
;; maintain two files, but if your server is rejecting your mail as
;; unauthenticated, be sure to check this.  For an Exchange server (used by my
;; university), the pass entry will look like this:
;;
;; $ pass edit auth-sources/mahe0054@outlook.office365.com
;; <password>
;; host: outlook.office365.com
;; user: mahe0054@uni.flinders.edu.au
;;
;; And for Gmail:
;;
;; $ pass edit auth-sources/eqyiel@imap.gmail.com
;; <password>
;; host: imap.gmail.com
;; user: eqyiel@gmail.com
;;
;; If you're using http://www.emacswiki.org/emacs/MultipleSMTPAccounts the
;; smtp-server and host field must match the third argument to an account in
;; `smtp-accounts'.  For example:
;;
;; (defvar smtp-accounts
;;   '((ssl "eqyiel@gmail.com" "imap.gmail.com" 587 "eqyiel@gmail.com" nil)
;;     (ssl "mahe0054@uni.flinders.edu.au" "outlook.office365.com" 587
;;          "mahe0054@uni.flinders.edu.au" nil)))

(require 'password-store)
(require 'auth-password-store)

(eval-after-load "auth-source"
  ;; Don't try to read from ~/.authinfo{,.gpg} or ~/.netrc
  '(setq auth-sources nil))

(auth-pass-enable)

(provide 'eqyiel-pass)
