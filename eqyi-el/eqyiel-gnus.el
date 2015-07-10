;;; eqyiel-gnus.el

(autoload 'gnus "gnus" nil t)

;; init from this file, not ~/.gnus
(eval-after-load "gnus"
  '(setq gnus-init-file "~/.emacs.d/eqyiel-gnus.el"))

(eval-after-load "gnus"
  '(setq gnus-startup-file "~/.config/gnus/newsrc" ;; don't pollute my homedir
         gnus-save-newsrc-file t
         gnus-read-newsrc-file t))

;; http://www.emacswiki.org/emacs/EasyPG#toc8

(eval-after-load "gnus"
  '(progn
     (require 'epg-config)
     (setq mml2015-use 'epg
           mml2015-verbose t
           epg-user-id "Ruben Maher"
           mml2015-encrypt-to-self t
           mml2015-always-trust nil
           mml2015-cache-passphrase t
           mml2015-passphrase-cache-expiry '3600
           mml2015-sign-with-sender t
           gnus-message-replyencrypt t
           gnus-message-replysign t
           gnus-message-replysignencrypted t
           gnus-treat-x-pgp-sig t
           mm-sign-option 'guided
           mm-encrypt-option 'guided
           mm-verify-option 'always
           mm-decrypt-option 'always
           gnus-buttonized-mime-types
           '("multipart/alternative"
             "multipart/encrypted"
             "multipart/signed")
           epg-debug t)))

(eval-after-load "mailcap"
  '(if (file-exists-p "~/.config/gnus/mailcap")
      (mailcap-parse-mailcap "~/.config/gnus/mailcap")))

;; oh behave
(eval-after-load "gnus"
  '(define-key gnus-summary-mode-map (kbd "S-SPC") 'gnus-summary-prev-page))

(eval-after-load "gnus"
  '(progn
     (setq
      gnus-activate-level 5
      gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n"
      gnus-summary-stop-at-end-of-message t
      gnus-auto-center-summary t
      gnus-auto-center-group t
      ;; don't ask for confirmation before entering a large newsgroup, just do it
      gnus-large-newsgroup nil
      gnus-large-ephemeral-newsgroup nil
      ;; don't save my killed groups, please, just kill them.
      gnus-save-killed-list nil
      ;; don't ask me about every single news group you find on the server
      gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
      gnus-gcc-mark-as-read t)))

(eval-after-load "gnus-start"
  ;; don't query the server and slow down my startup
  '(setq gnus-check-new-newsgroups 'ask-server))

;; use topic headings for groups, like [ Gnus ], [ misc ], etc.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; show when I last read a group.
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; when in message mode, wrap the text according to `fill-column'.
(add-hook 'message-mode-hook 'turn-off-auto-fill)
(add-hook 'message-mode-hook 'turn-on-visual-line-mode)
(add-hook 'message-mode-hook 'turn-on-visual-fill-column-mode)

;; allow attaching files from dired
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(eval-after-load "gnus"
  '(setq gnus-select-method
         ;; First argument to nnimap should match name of some file in
         ;; ~/.password-store/.
         '(nnimap "eqyiel@localhost"
                  (nnimap-address "localhost")
                  (nnimap-stream network)
                  (nnir-search-engine imap))))
;; (eval-after-load "gnus"
;;   '(setq gnus-select-method
;;          ;; First argument to nnimap should match name of some file in
;;          ;; ~/.password-store/.
;;          '(nnmaildir ""
;;                   (directory "/home/eqyiel/nnmaildir/")
;;                   (nnmaildir-directory "/home/eqyiel/nnmaildir/")
;;                   (get-new-mail nil)
;;                   (nnir-search-engine notmuch)
;;                   )
;;          nnir-notmuch-remove-prefix "/home/eqyiel/"
;;          )
;;   )

;; (setq gnus-select-method
;;       '(nnmaildir ""
;;         (directory "~/Maildir/")
;;         (get-new-mail nil)
;;         (nnir-search-engine notmuch))
;;       nnir-notmuch-remove-prefix "/home/wgg/Maildir/")

;; (defun wg/update-notmuch ()
;;   (start-process "notmuch-poll" nil "notmuch" "new"))
;; (add-hook 'gnus-after-getting-new-news-hook 'wg/update-notmuch)
;; (add-hook 'gnus-summary-exit-hook 'wg/update-notmuch)


;; For remote:
;; (eval-after-load "gnus"
;;   '(setq gnus-select-method
;;          '(nnimap "rkm.id.au"
;;                   (nnimap-address "rkm.id.au")
;;                   (nnimap-server-port "imaps")
;;                   (nnimap-stream ssl))
;;          gnus-secondary-select-methods
;;          '((nnimap "imap.gmail.com"
;;                    (nnimap-address "imap.gmail.com")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl))
;;            (nnimap "mail.internode.on.net"
;;                    (nnimap-address "mail.internode.on.net")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl))
;;            (nnimap "outlook.office365.com"
;;                    (nnimap-address "outlook.office365.com")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl))
;;            (nnimap "huttriverprovince.com.au"
;;                    (nnimap-address "huttriverprovince.com.au")
;;                    (nnimap-server-port 993)
;;                    (nnimap-stream ssl)))))

;; Don't use NNTP aymore, but for future reference this is how to do it:
;; (eval-after-load "gnus"
;;   '(setq gnus-secondary-select-methods
;;          '((nntp "localhost"))))

(eval-after-load "message"
  '(setq message-kill-buffer-on-exit t
         ;; have the name show up, please.
         message-from-style 'angles
         ;; add Cc and Bcc headers to the message buffer
         message-default-mail-headers "Cc: \nBcc: \n"
         message-generate-headers-first t
         message-default-charset 'utf-8
         message-cite-function 'message-cite-original-without-signature
         ))

;; Let Gnus change the "From:" line by looking at current group we are in. First
;; argument to each posting style is the name of a group in Gnus, not the server
;; itself!
(eval-after-load "gnus"
  '(setq gnus-posting-styles
         '(("gmail.com" (address "eqyiel@gmail.com") (name "Ruben Maher"))
           ("flinders.edu.au" (address "mahe0054@uni.flinders.edu.au")
            (name "Ruben Maher"))
           ("rkm.id.au" (address "r@rkm.id.au") (name "Ruben Maher"))
           ("huttriverprovince.com.au" (address "info@huttriverprovince.com.au")
            (name "Hutt River Province"))
           ("internode.on.net" (address "eqyiel@internode.on.net")
            (name "Ruben Maher")))))

;; Each component of smtp-accounts has the form
;; (protocol  "adres_matched_in_From_field@foo.com"
;;            "protocol.foo.com"
;;            "port"
;;            "user@foo.com"
;;            "password"
;;            "key"
;;            "cert")

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
(defvar smtp-accounts
  '((ssl "eqyiel@gmail.com" "imap.gmail.com" 587 "eqyiel@gmail.com" nil)
    (ssl "mahe0054@uni.flinders.edu.au" "outlook.office365.com" 587
         "mahe0054@uni.flinders.edu.au" nil) ;; flinders now uses office365
    (ssl "r@rkm.id.au" "rkm.id.au" 587 "r@rkm.id.au" nil)
    (ssl "eqyiel@internode.on.net" "mail.internode.on.net" 25
         "eqyiel@internode.on.net" nil)
    (ssl "info@huttriverprovince.com.au" "rkm.id.au" 587
         "info@huttriverprovince.com.au" nil)))

(eval-after-load "gnus"
  ;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
  '(progn
     (require 'cl) ; `change-smtp' uses `cl-loop'
     (require 'smtpmail)
     (setq send-mail-function 'smtpmail-send-it
           message-send-mail-function 'smtpmail-send-it
           mail-from-style nil
           smtpmail-debug-info t
           smtpmail-debug-verb t)))

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (message (concat "from set-smtp " password))
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (message (concat "from set-smtp-ssl " password))
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain login))
               (return (apply 'set-smtp (cons auth-mech auth-spec))))
              ((eql auth-mech 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
          finally (error "Cannot infer SMTP information."))))

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
(defadvice smtpmail-via-smtp (before smtpmail-via-smtp-ad-change-smtp
                                     (recipient smtpmail-text-buffer
                                                &optional ask-for-password))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

(eval-after-load "gnus"
  '(setq starttls-use-gnutls t
         message-directory "~/mail/"
         mail-source-directory "~/mail/"
         gnus-agent-directory "~/news/"
         gnus-article-save-directory "~/news/"
         gnus-cache-directory "~/news/"
         gnus-directory "~/news/"
         gnus-home-directory "~/"
         gnus-kill-files-directory "~/news/"
         gnus-summary-display-arrow t
         gnus-refer-thread-limit t
         gnus-fetch-old-headers 'some
         ;; gnus-fetch-old-headers t
         gnus-uu-be-dangerous t))

(eval-after-load "gnus"
  (setq-default
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-sort-functions '(gnus-thread-sort-by-date)
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "\u251c\u25ba "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "\u2570\u25ba "
   gnus-sum-thread-tree-vertical "\u2502"))

;;; Show images inline
(eval-after-load "gnus"
  '(progn
     (setq gnus-inhibit-images nil)
     (setq gnus-blocked-images nil)
     (setq mm-text-html-renderer 'shr)
     (setq gnus-safe-html-newsgroups "\\`nnimap[+:]")
     (setq mm-inline-text-html-with-images t)
     (setq mm-inline-large-images-proportion 0.5)
     (setq shr-blocked-images nil)))

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/richtext")
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-attachment-override-types "image/*")
     (setq mm-inline-large-images 'resize)))

;; stop message-mode from adding the Fcc: header when notmuch is loaded.
(eval-after-load "notmuch"
  (setq notmuch-fcc-dirs nil))

;; In case I accidentally my newsrc.eld
;; (eval-after-load "gnus-group"
;;   '(progn
;;      (setq gnus-topic-topology
;;            '(("Gnus" visible nil nil)
;;              (("rkm.id.au" visible nil ((gcc-self . "rkm.id.au/Sent"))))
;;              (("internode.on.net" visible nil
;;                ((gcc-self . "internode.on.net/Sent"))))
;;              (("flinders.edu.au" visible nil
;;                ((gcc-self . "flinders.edu.au/Sent"))))
;;              (("gmail.com" visible nil ((gcc-self . "gmail.com/Sent"))))
;;              (("huttriverprovince.com.au" visible nil
;;                ((gcc-self . "huttriverprovince.com.au/Sent")))))
;;            gnus-topic-alist
;;            '(("Gnus" "archive" "nndraft:drafts")
;;              ("flinders.edu.au"
;;               "flinders.edu.au/Bin"
;;               "flinders.edu.au/Keep"
;;               "flinders.edu.au/New"
;;               "flinders.edu.au/Sent"
;;               "flinders.edu.au/Spam")
;;              ("gmail.com"
;;               "gmail.com/Bin"
;;               "gmail.com/New"
;;               "gmail.com/Spam"
;;               "gmail.com/Sent")
;;              ("huttriverprovince.com.au"
;;               "huttriverprovince.com.au/New"
;;               "huttriverprovince.com.au/Sent"
;;               "huttriverprovince.com.au/Spam"
;;               "huttriverprovince.com.au/Trash")
;;              ("internode.on.net"
;;               "internode.on.net/Bin"
;;               "internode.on.net/Drafts"
;;               "internode.on.net/New"
;;               "internode.on.net/Sent"
;;               "internode.on.net/Spam")
;;              ("rkm.id.au"
;;               "rkm.id.au/Keep"
;;               "rkm.id.au/New"
;;               "rkm.id.au/RSS"
;;               "rkm.id.au/Sent"
;;               "rkm.id.au/Spam"
;;               "rkm.id.au/sa-learn"
;;               "rkm.id.au/sa-unlearn")))))

(provide 'eqyiel-gnus)
