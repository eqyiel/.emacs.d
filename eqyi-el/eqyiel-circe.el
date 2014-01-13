;;; eqyiel-circe.el

(require 'circe)
(require 'circe-color-nicks)

(enable-circe-color-nicks)

(if (file-exists-p "~/.private.el.gpg")
    (load-file "~/.private.el.gpg")
  (setq eqyiel/freenode-pw nil
        ;; eqyiel/bitlbee-pw nil
        ;; eqyiel/austnet-pw nil
        eqyiel/oftc-pw nil
        ;; eqyiel/mozilla-pw nil
        eqyiel/rizon-pw nil))

(setq circe-network-options
      `(("freenode"
         :nick "eqyiel"
         :host "rkm.id.au"
         :service "7777"
         :tls t
         :pass ,eqyiel/freenode-pw)
        ;; ("bitlbee"
        ;;  :nick "eqyiel"
        ;;  :host "rkm.id.au"
        ;;  :service "7777"
        ;;  :tls t
        ;;  :pass ,eqyiel/bitlbee-pw)
        ;; ("austnet"
        ;;  :nick "eqyiel"
        ;;  :host "rkm.id.au"
        ;;  :service "7777"
        ;;  :tls t
        ;;  :pass ,eqyiel/austnet-pw)
        ("oftc"
         :nick "eqyiel"
         :host "rkm.id.au"
         :service "7777"
         :tls t
         :pass ,eqyiel/oftc-pw)
        ;; ("mozilla"
        ;;  :nick "eqyiel"
        ;;  :host "rkm.id.au"
        ;;  :service "7777"
        ;;  :tls t
        ;;  :pass ,eqyiel/mozilla-pw)
        ;; ("rizon"
        ;;  :nick "eqyiel"
        ;;  :host "rkm.id.au"
        ;;  :service "7777"
        ;;  :tls t
        ;;  :pass ,eqyiel/rizon-pw)
        ))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "freenode")
  ;; (circe "austnet")
  ;; (circe "bitlbee")
  (circe "oftc")
  ;; (circe "mozilla")
  ;; (circe "rizon")
  )

(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M"
 lui-flyspell-p t
 lui-max-buffer-size 10000
 lui-fill-column 70
 lui-fill-type 'variable
 lui-highlight-keywords '("eqyiel"))

(setq
 circe-default-quit-message "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
 circe-default-part-message "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ"
 circe-highlight-nick-type 'all
 circe-color-nicks-everywhere t
 ;; circe-reduce-lurker-spam t ;; sometimes, I want to see this
 circe-reduce-lurker-spam t
 circe-format-say "<{nick}> {body}"
 circe-format-self-say "<{nick}> {body}")

;; see if this is not too annoying
;; (setq tracking-ignored-buffers '(("#archlinux" circe-highlight-nick-face)
;;                                  ("##japanese" circe-highlight-nick-face)
;;                                  ("#archaudio" circe-highlight-nick-face)
;;                                  ("#archlinux-bugs" circe-highlight-nick-face)
;;                                  ("#archlinux-offtopic"
;;                                   circe-highlight-nick-face)
;;                                  ("#conkeror" circe-highlight-nick-face)
;;                                  ("#emacs" circe-highlight-nick-face)
;;                                  ("#emacs-circe" circe-highlight-nick-face)
;;                                  ("#git" circe-highlight-nick-face)
;;                                  ("#gnus" circe-highlight-nick-face)
;;                                  ("#python" circe-highlight-nick-face)
;;                                  ("#tilaa" circe-highlight-nick-face)
;;                                  ("#webpy" circe-highlight-nick-face)
;;                                  ("##c++" circe-highlight-nick-face)))

(setq lui-flyspell-alist '(("." "en_US")))

(add-hook 'circe-mode-hook (lambda ()
    (remove-from-invisibility-spec 'lui-fool)))

;;; works, nothing against these guys though
;; (setq circe-fool-list '("OpenEye" "SimpleBlue"))

(add-hook 'circe-channel-mode-hook 'turn-on-flyspell)

(defun my-circe-set-margin ()
  (setq right-margin-width 5))

(add-hook 'lui-mode-hook 'my-circe-set-margin)

(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                      -CAfile ~/.config/certs/znc.crt"
                    "gnutls-cli --insecure -p %p %h"
                    "gnutls-cli --insecure -p %p %h --protocols ssl3"))

;; https://github.com/jorgenschaefer/circe/wiki/Configuration
(require 'notifications)
(require 's)

(defun notifications-close-notification (id &optional bus)
  "Close a notification with identifier ID.
BUS can be a string denoting a D-Bus connection, the default is `:session'."
  (dbus-call-method (or bus :session)
                    notifications-service
                    notifications-path
                    notifications-interface
                    notifications-close-notification-method
                    :uint32 id))

(defvar tom/chatnotification nil
  "ID of the last send desktop notification.")
(defvar tom/lastchatnotification 0
  "Time of the last send notification, seconds since epoch as float")
(defvar tom/lastbufferlist nil
  "The value of tracking-buffers when we last notified")
(defvar tom/chatnotifyintervall 90
  "Minimum delay between chat activity notifications in seconds")

(defadvice tracking-add-buffer (after tracking-desktop-notify activate)
  (let ((current-t (float-time))
        (current-bl (s-join "\n" tracking-buffers)))
    ;; min tom/chatnotifyintervall seconds since last delay?
    (if (and (not (eql current-bl "")) (not (eql current-bl tom/lastbufferlist))
             (> (- current-t tom/lastchatnotification) tom/chatnotifyintervall))
        (progn
          ;; delete alst notification id any
          (and tom/chatnotification (notifications-close-notification tom/chatnotification))
          ;; remember time and notify
          (setq  tom/lastchatnotification current-t
                 tom/lastbufferlist current-bl
                 tom/chatnotification (notifications-notify
                                       :title "Emacs Active Buffers"
                                       :body current-bl
                                       :timeout 750
                                       :desktop-entry "emacs24"
                                       :sound-name "message-new-entry"
                                       :transient))))))

;; what I want to do eventually
;; (defadvice circe-server-handler (after my-test activate)
;;   (if (s-contains-p "eqyiel" line)
;;       (let* ((parsed (circe-server-parse-line
;;                       (decode-coding-string line
;;                                             (if (consp circe-server-coding-system)
;;                                                 (cdr circe-server-coding-system)
;;                                               circe-server-coding-system)
;;                                             t)))
;;              (nick (aref parsed 0))
;;              (user (aref parsed 1))
;;              (host (aref parsed 2))
;;              (command (aref parsed 3))
;;              (args (aref parsed 4)))
;;         ;; (setq flup args)
;;         (notifications-notify
;;          :title "lol"
;;          :body line
;;          :timeout 750
;;          :desktop-entry "emacs24"
;;          :sound-name "message-new-entry"
;;          :transient))
;;     (message "nope")))

;;  working:
;; how to check against each `lui-highlight-keywords'?

;; (defadvice circe-server-handler (after my-test activate)
;;   (let* ((parsed (circe-server-parse-line
;;                   (decode-coding-string line
;;                                         (if (consp circe-server-coding-system)
;;                                             (cdr circe-server-coding-system)
;;                                           circe-server-coding-system)
;;                                         t)))
;;          (nick (aref parsed 0))
;;          (user (aref parsed 1))
;;          (host (aref parsed 2))
;;          (command (aref parsed 3))
;;          (args (aref parsed 4)))
;;     (if (cdr-safe args)
;;         (notifications-notify
;;          :title "lol"
;;          :body (concat nick ": " (cadr args))
;;          :timeout 750
;;          :desktop-entry "emacs24"
;;          :sound-name "message-new-entry"
;;          :transient)
;;         ;; (message (concat nick ": " (cadr args)))
;;         )))

;; (s-contains? "needle" "needle in the haystack")
;; (setq things '("the" "owls" "are" "not" "what" "they" "seem"))
;; (cdr things)
;; (cdr-safe things)
;; (concat "" (cadr things))
(provide 'eqyiel-circe)
