;;; eqyiel-circe.el

(defun irc ()
  "Connect to IRC."
  (interactive)
  (eqyiel-circe-setup-networks)
  (circe "freenode")
  (circe "oftc"))

(defun eqyiel-circe-reconnect-all () (interactive) (circe-reconnect-all))

(defun eqyiel-circe-setup-networks ()
  (require 'circe)
  (setq circe-network-options
        ;; A list quoted with a backtick is quoted except for the elements
        ;; prefixed with a comma, which are evaluated before inserting.
        `(("freenode"
           :nick "eqyiel"
           :host "rkm.id.au"
           :service "7777"
           :tls t
           :pass ,(concat "eqyiel/freenode:" (password-store-get "irc/znc")))
          ("oftc"
           :nick "eqyiel"
           :host "rkm.id.au"
           :service "7777"
           :tls t
           :pass ,(concat "eqyiel/oftc:" (password-store-get "irc/znc"))))))
(advice-add 'circe-reconnect-all :before 'eqyiel-circe-setup-networks)

(defun eqyiel-circe-clear-passwords ()
  (if (boundp 'circe-network-options)
      (dolist (network circe-network-options)
        (plist-put (cdr network) :pass nil))))

(defvar eqyiel-circe-authentications-count 0
  "Clear passwords after this many authentications have been seen.")

(defun eqyiel-circe-wait-for-authentication ()
  (setq eqyiel-circe-authentications-count
        (+ 1 eqyiel-circe-authentications-count))
  (unless (> (list-length circe-network-options)
             eqyiel-circe-authentications-count)
    (progn
      (eqyiel-circe-clear-passwords)
      (setq eqyiel-circe-authentications-count 0))))
(add-hook 'circe-server-connected-hook 'eqyiel-circe-wait-for-authentication)

(eval-after-load 'circe
  '(progn
     (require 'circe-chanop)
     (require 'circe-color-nicks)
     (setq circe-default-quit-message
           "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
           circe-default-part-message
           "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
           circe-highlight-nick-type 'all
           circe-reduce-lurker-spam t ;; sometimes, I want to see this
           circe-format-say "<{nick}> {body}"
           circe-format-self-say "<{nick}> {body}"
           circe-color-nicks-everywhere t)
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

(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))

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

(add-hook 'circe-receive-message-functions 'eqyiel-circe-wait-for-znc)

(defun eqyiel-disable-circe-notifications ()
  (setq eqyiel-circe-znc-notices 0)
  (remove-hook 'circe-receive-message-functions 'circe-notifications)
  (add-hook 'circe-receive-message-functions 'eqyiel-circe-wait-for-znc))

(advice-add 'circe-reconnect-all :before 'eqyiel-disable-circe-notifications)

(provide 'eqyiel-circe)
