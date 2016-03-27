;;; eqyiel-gnus.el

(autoload 'gnus "gnus" nil t)

(eval-after-load "gnus"
  '(progn
     (require 'epg-config)
     (setq gnus-startup-file "~/.config/gnus/newsrc"
           gnus-save-newsrc-file t
           gnus-read-newsrc-file t
           gnus-activate-level 5
           gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n"
           gnus-summary-stop-at-end-of-message t
           gnus-auto-center-summary t
           gnus-auto-center-group t
           gnus-large-newsgroup nil
           gnus-large-ephemeral-newsgroup nil
           gnus-save-killed-list nil
           gnus-summary-save-parts-default-mime ".*/.*"
           gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
           gnus-gcc-mark-as-read t
           mml2015-use 'epg
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
           epg-debug t)
     (define-key gnus-summary-mode-map (kbd "S-SPC") 'gnus-summary-prev-page)))

;; Exclude message itself when saving all attachments
;; http://stackoverflow.com/a/8416710
(defadvice gnus-summary-save-parts-1
    (around gnus-summary-save-parts-exclude-self activate)
  (let ((handle (ad-get-arg 2)))
    (unless (and (not (stringp (car handle)))
                 (not (mm-handle-filename handle)))
      ad-do-it)))

(eval-after-load "mailcap"
  '(if (file-exists-p "~/.config/gnus/mailcap")
      (mailcap-parse-mailcap "~/.config/gnus/mailcap")))

(eval-after-load "gnus-start"
  '(setq gnus-check-new-newsgroups 'ask-server))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(add-hook 'message-mode-hook 'turn-off-auto-fill)
(add-hook 'message-mode-hook 'turn-on-visual-line-mode)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(eval-after-load "gnus"
  '(if (string-equal (system-name) "ayanami.rkm.id.au")
       (setq gnus-select-method
             ;; First argument to nnimap should match name of some file in
             ;; ~/.password-store/.
             '(nnimap "eqyiel@localhost"
                      (nnimap-address "localhost")
                      (nnimap-stream network)
                      (nnir-search-engine imap)))
     (setq gnus-select-method
           '(nnimap "rkm.id.au"
                    (nnimap-address "rkm.id.au")
                    (nnimap-server-port "imaps")
                    (nnimap-stream ssl))
           gnus-secondary-select-methods
           '((nnimap "imap.gmail.com"
                     (nnimap-address "imap.gmail.com")
                     (nnimap-server-port 993)
                     (nnimap-stream ssl))
             (nnimap "mail.internode.on.net"
                     (nnimap-address "mail.internode.on.net")
                     (nnimap-server-port 993)
                     (nnimap-stream ssl))
             (nnimap "outlook.office365.com"
                     (nnimap-address "outlook.office365.com")
                     (nnimap-server-port 993)
                     (nnimap-stream ssl))
             (nnimap "huttriverprovince.com.au"
                     (nnimap-address "huttriverprovince.com.au")
                     (nnimap-server-port 993)
                     (nnimap-stream ssl))))))

(eval-after-load "message"
  '(setq message-kill-buffer-on-exit t
         message-from-style 'angles
         message-default-mail-headers "Cc: \nBcc: \n"
         message-generate-headers-first t
         message-default-charset 'utf-8
         message-cite-function 'message-cite-original-without-signature))

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
            (name "Ruben Maher"))
           ("mangochutney.com.au" (address "ruben@mangochutney.com.au")
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
         "info@huttriverprovince.com.au" nil)
    (ssl "ruben@mangochutney.com.au" "imap.gmail.com" 587
         "ruben@mangochutney.com.au" nil)))

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
        smtpmail-smtp-user user
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
        smtpmail-smtp-user user
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

;; example from wgreenhouse, come back to this at some point
;; ;; Basic gnus nnmaildir config integrating notmuch.
;; ;;
;; ;; Out of scope: message retrieval (fetchmail), splitting/filtering (procmail),
;; ;; sending (message-mode/smtpmail.el + gnus posting styles).

;; (setq gnus-select-method
;;       `(nnmaildir ""
;;                   (directory ,(expand-file-name "~/Maildir/")) ;; Top level of your dir-of-maildirs
;;                   (get-new-mail nil)
;;                   (nnir-search-engine notmuch))
;;       gnus-secondary-select-methods
;;       '((nntp "gmane"
;;               (nntp-open-connection-function nntp-open-tls-stream)
;;               (nntp-port-number 563)
;;               (nntp-address "news.gmane.org"))
;;         ;; (nntp "aioe"
;;         ;;       (nntp-prepare-server-hook
;;         ;;        ((lambda ()
;;         ;;           (call-process "ssh" nil nil nil
;;         ;;                         "-L 5630:nntp.aioe.org:563"
;;         ;;                         "digit"
;;         ;;                         "-n"))))
;;         ;;       (nntp-open-connection-function nntp-open-tls-stream)
;;         ;;       (nntp-port-number 5630)
;;         ;;       (nntp-address "127.0.0.1"))
;;         (nntp "eternal-september"
;;               (nntp-open-connection-function nntp-open-tls-stream)
;;               (nntp-port-number 443)
;;               (nntp-address "news.eternal-september.org")))
;;       gnus-message-archive-group "nnmaildir:mail"
;;       gnus-parameters
;;       '((".+"
;;          (gcc-self . t)))
;;       nnir-notmuch-remove-prefix
;;       (expand-file-name "~/Maildir/") ;; This should be the root of your dir-of-maildirs
;;       gnus-use-full-window nil
;;       mm-discouraged-alternatives
;;       '("text/html")
;;       mm-text-html-renderer 'gnus-w3m
;;       gnus-inhibit-mime-unbuttonizing t
;;       gnus-treat-body-boundary nil
;;       gnus-summary-line-format
;;       (concat
;;        "%U%R%z"
;;        "%-20,20f" ;; name
;;        "  "
;;        "%3{│%}" "%-25,25&user-date;" "%3{│%}" ;; date
;;        "  "
;;        "%B"
;;        "%s\n")
;;       gnus-summary-display-arrow t
;;       message-send-mail-function 'message-smtpmail-send-it
;;       mail-user-agent 'gnus-user-agent
;;       gnus-use-adaptive-scoring t
;;       gnus-home-score-file "~/.emacs.d/gnus.score"
;;       gnus-home-adapt-file "~/.emacs.d/gnus.adapt"
;;       gnus-asynchronous t
;;       gnus-thread-hide-subtree t
;;       mml2015-encrypt-to-self t
;;       gnus-blocked-images ".*")

;; ;; We need this because otherwise notmuch points at nonexistent files
;; ;; because Gnus moves from new/ to cur/ on start.

;; (add-hook 'gnus-after-getting-new-news-hook
;;           (lambda
;;             ()
;;             (start-process "update-notmuch" nil "notmuch" "new")))

;; ;; Use something like this to track your primary/priority inbox in
;; ;; display-time-mode. This is especially handy if you use some kind of
;; ;; asynchronous background mail retrieval (e.g. fetchmail+procmail).

;; (setq display-time-mail-directory (expand-file-name "~/Maildir/mail/new/")
;;       display-time-use-mail-icon t
;;       read-mail-command 'gnus)

(provide 'eqyiel-gnus)
