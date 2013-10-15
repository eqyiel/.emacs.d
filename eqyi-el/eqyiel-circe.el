;;; eqyiel-circe.el

(require 'circe)
(require 'circe-color-nicks)

(enable-circe-color-nicks)

(if (file-exists-p "~/.private.el")
    (load-file "~/.private.el")
  (setq eqyiel/freenode-pw nil
        eqyiel/bitlbee-pw nil
        eqyiel/austnet-pw nil))

(setq circe-network-options
      `(("freenode"
         :nick "eqyiel"
         :host "rkm.id.au"
         :service "7777"
         :tls t
         :pass ,eqyiel/freenode-pw)
        ("bitlbee"
         :nick "eqyiel"
         :host "rkm.id.au"
         :service "7777"
         :tls t
         :pass ,eqyiel/bitlbee-pw)
        ("austnet"
         :nick "eqyiel"
         :host "rkm.id.au"
         :service "7777"
         :tls t
         :pass ,eqyiel/austnet-pw)))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "freenode")
  (circe "austnet"))

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
 circe-reduce-lurker-spam nil
 circe-format-say "<{nick}> {body}"
 circe-format-self-say "<{nick}> {body}")

(setq tracking-ignored-buffers '(("#archlinux" circe-highlight-nick-face)
                                 ("##japanese" circe-highlight-nick-face)
                                 ("#archaudio" circe-highlight-nick-face)
                                 ("#archlinux-bugs" circe-highlight-nick-face)
                                 ("#archlinux-offtopic"
                                  circe-highlight-nick-face)
                                 ("#conkeror" circe-highlight-nick-face)
                                 ("#emacs" circe-highlight-nick-face)
                                 ("#emacs-circe" circe-highlight-nick-face)
                                 ("#git" circe-highlight-nick-face)
                                 ("#gnus" circe-highlight-nick-face)
                                 ("#python" circe-highlight-nick-face)
                                 ("#tilaa" circe-highlight-nick-face)
                                 ("#webpy" circe-highlight-nick-face)
                                 ("##c++" circe-highlight-nick-face)))

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

(provide 'eqyiel-circe)
