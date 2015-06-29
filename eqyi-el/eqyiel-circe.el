;;; eqyiel-circe.el

(require 'circe-autoloads)

(if (file-exists-p "~/.private.el.gpg")
    (load-file "~/.private.el.gpg")
  (setq eqyiel/freenode-pw nil
        eqyiel/oftc-pw nil))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "freenode")
  (circe "oftc"))

(eval-after-load 'circe
  '(progn
     (require 'circe-chanop)
     (require 'circe-color-nicks)
     (setq circe-default-quit-message "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
           circe-default-part-message "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
           circe-highlight-nick-type 'all
           circe-reduce-lurker-spam t ;; sometimes, I want to see this
           circe-format-say "<{nick}> {body}"
           circe-format-self-say "<{nick}> {body}"
           circe-color-nicks-everywhere t
           circe-network-options
           `(("freenode"
              :nick "eqyiel"
              :host "rkm.id.au"
              :service "7777"
              :tls t
              :pass ,eqyiel/freenode-pw)
             ("oftc"
              :nick "eqyiel"
              :host "rkm.id.au"
              :service "7777"
              :tls t
              :pass ,eqyiel/oftc-pw)))
     (enable-circe-color-nicks)))

(eval-after-load "lui"
  '(setq lui-highlight-keywords '("eqyiel")
         lui-time-stamp-position 'right-margin
         lui-time-stamp-format "%H:%M"
         lui-flyspell-p t
         lui-max-buffer-size 10000
         lui-fill-column 70
         lui-fill-type 'variable
         lui-flyspell-alist '(("." "en_US"))))

(add-hook 'circe-channel-mode-hook 'turn-on-flyspell)

(defun eqyiel-show-fools ()
  (remove-from-invisibility-spec 'lui-fool))

(add-hook 'circe-mode-hook 'eqyiel-show-fools)

(defun eqyiel-circe-set-margin ()
  (setq right-margin-width 5))

(add-hook 'lui-mode-hook 'eqyiel-circe-set-margin)

;; (setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
;;                                       -CAfile ~/.config/certs/znc.crt"
;;                     "gnutls-cli --insecure -p %p %h"
;;                     "gnutls-cli --insecure -p %p %h --protocols ssl3"))

(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))

;; (define-abbrev-table circe-channel-mode-abbrev-table nil)

(autoload 'enable-circe-notifications "circe-notifications" nil t)

(eval-after-load "circe-notifications"
  '(setq circe-notifications-watch-nicks
         '("eqyiel" "nyarlu" "eqyiel1" "fthagn" "forcer")
         circe-notifications-check-window-focus t
         circe-notifications-wait-for 30))

;; warning: this is very dumb

(defvar eqyiel-circe-znc-notices 0
  "How many notices have we received from ZNC?")

(defvar eqyiel-circe-znc-motd-length 25
  "How many lines are in ZNC's MOTD?")

(defun eqyiel-circe-znc-count-networks ()
  "Return the number of networks in `circe-network-options' multiplied by
`eqyiel-circe-znc-motd-length', so we can know how many notices to expect before
enabling notifications."
  (* eqyiel-circe-znc-motd-length (list-length circe-network-options)))

(defun eqyiel-circe-wait-for-znc (nick user host command args)
  (if (> (eqyiel-circe-znc-count-networks) eqyiel-circe-znc-notices)
      (when (and (string-equal host "znc.in")
               (string-equal command "NOTICE"))
        (setq eqyiel-circe-znc-notices (+ 1 eqyiel-circe-znc-notices))
        (message "That's %d ..." eqyiel-circe-znc-notices))
    (progn
      (message "OK.")
      (remove-hook 'circe-receive-message-functions 'eqyiel-circe-wait-for-znc)
      (enable-circe-notifications))))

(defadvice circe-reconnect (before eqyiel-remove-circe-notifications)
  (setq eqyiel-circe-znc-notices 0)
  (remove-hook 'circe-receive-message-functions 'circe-notifications)
  (add-hook 'circe-receive-message-functions 'eqyiel-circe-wait-for-znc))

(add-hook 'circe-receive-message-functions 'eqyiel-circe-wait-for-znc)

(provide 'eqyiel-circe)
