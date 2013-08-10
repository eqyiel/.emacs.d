;;; eqyiel-gnus.el

(require 'gnus)

;; http://www.emacswiki.org/emacs/EasyPG#toc8

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

      epg-debug t)

;; init from this file, _not_ ~/.gnus
(setq gnus-init-file "~/.emacs.d/eqyiel-gnus.el")

(eval-after-load "mailcap"
  '(progn
     (mailcap-parse-mailcap "~/.config/gnus/mailcap")))

;; oh behave
(define-key gnus-summary-mode-map (kbd "S-SPC") 'gnus-summary-prev-page)

;; for levels below this the server will not be queried
(setq gnus-activate-level 5)

;; use topic headings for groups, like [ Gnus ], [ misc ], etc.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; show when I last read a group.
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n")

(setq gnus-summary-stop-at-end-of-message t
      gnus-auto-center-summary t
      gnus-auto-center-group t)

;; don't ask for confirmation before entering a large newsgroup, just do it
(setq gnus-large-newsgroup nil
      gnus-large-ephemeral-newsgroup nil)

;; I don't need another newsreader, but these might be handy if I kill my
;; el-dingo file
(setq gnus-startup-file "~/.config/gnus/newsrc" ;; don't pollute my homedir
      gnus-save-newsrc-file t
      gnus-read-newsrc-file t)

;; don't save my killed groups, please, just kill them.
(setq gnus-save-killed-list nil)

;; don't query the server and slow down my startup
(setq gnus-check-new-newsgroups 'ask-server)

;; don't ask me about every single news group you find on the server, ffs!!
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)

;; when in message mode, wrap the text according to `fill-column'.
(add-hook 'message-mode-hook
          '(lambda ()
             (turn-on-auto-fill)))

(setq gnus-select-method
      '(nnimap "dovecot"
               (nnimap-address "localhost")
               (nnimap-stream network)))

;; (setq gnus-secondary-select-methods
;;              '((nntp "news.internode.on.net")
;;                (nntp "news.giganews.com")))

(add-to-list 'gnus-secondary-select-methods '(nntp "localhost"))

;; http://notmuchmail.org/emacstips/
;; One annoying standard configuration of message mode is that it will hide the
;; sent mail in your emacs frame stack, but it will not close it. If you type
;; several mails in an emacs session they will accumulate and make switching
;; between buffers more annoying. You can avoid that behavior by adding (setq
;; message-kill-buffer-on-exit t) in your .emacs file (or doing M-x
;; customize-variable<RET>message-kill-buffer-on-exit<RET>) which will really
;; close the mail window after sending it.

(setq message-kill-buffer-on-exit t)

;; http://www.emacswiki.org/emacs/MultipleSMTPAccounts
;; Let Gnus change the "From:" line by looking at current group we are in.

;; For sending via flinders account, must quote the entire address when prompted
;; for username, and also put the same thing in from field.  Mystifying.

(setq gnus-posting-styles
      '(("gmail.com"        (address "eqyiel@gmail.com")
         (name "Ruben Maher"))
        ("flinders.edu.au"  (address "mahe0054@uni.flinders.edu.au")
         (name "Ruben Maher"))
        ("rkm.id.au"        (address "r@rkm.id.au")
         (name "Ruben Maher"))
        ("internode.on.net" (address "eqyiel@internode.on.net")
         (name "Ruben Maher"))))

;; Each component of smtp-accounts has the form
;; (protocol  "adres_matched_in_From_field@foo.com"
;;            "protocol.foo.com"
;;            "port"
;;            "user@foo.com"
;;            "password"
;;            "key"
;;            "cert")

(defvar smtp-accounts
  '((ssl "eqyiel@gmail.com" "smtp.gmail.com" 587 "eqyiel@gmail.com" nil)
    (ssl "mahe0054@uni.flinders.edu.au" "pod51003.outlook.com" 587
         "mahe0054@uni.flinders.edu.au" nil)
    (ssl "r@rkm.id.au" "rkm.id.au" 587 "r@rkm.id.au" nil)
    (ssl "eqyiel@internode.on.net" "mail.internode.on.net" 465
         "eqyiel@internode.on.net" nil)))

;; have the name show up, please.
(require 'message)
(setq message-from-style 'angles)

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; default smtpmail.el configurations.
(require 'cl)
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Ruben Maher"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
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

(defadvice smtpmail-via-smtp
  (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer &optional ask-for-password))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

(setq starttls-use-gnutls t
      message-directory "~/mail/"
      mail-source-directory "~/mail/"
      gnus-agent-directory "~/news/"
      gnus-article-save-directory "~/news/"
      gnus-cache-directory "~/news/"
      gnus-directory "~/news/"
      gnus-home-directory "~/"
      gnus-kill-files-directory "~/news/"
      gnus-summary-display-arrow t
      gnus-uu-be-dangerous t)

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
 gnus-sum-thread-tree-vertical "\u2502")

(require 'mm-url)
(require 'mm-decode)


;;; stuff to get images to show inline
(setq gnus-inhibit-images nil)
(setq gnus-blocked-images nil)
(setq mm-text-html-renderer 'shr)
(setq gnus-safe-html-newsgroups "\\`nnimap[+:]")
(setq mm-inline-text-html-with-images t)
(setq mm-inline-large-images-proportion 0.5)
(setq shr-blocked-images nil)

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")
     (add-to-list 'mm-attachment-override-types "image/*")
     (setq mm-inline-large-images 'resize)))

;; http://www.gnus.org/manual/gnus_398.html#SEC461
;; If you don't want HTML rendered, even if there's no text alternative add
;; (setq mm-automatic-display (remove "text/html" mm-automatic-display))

;; http://sachachua.com/blog/2007/12/gnus-multi-pane-tricks-or-i-heart-planet-emacsen/
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 60 (group 1.0))
               (vertical 1.0
                         (summary 0.20 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 60 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

(provide 'eqyiel-gnus)
