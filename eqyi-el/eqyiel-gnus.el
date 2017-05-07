;;; eqyiel-gnus.el

;; (when (string-equal (system-name) "ayanami")
;;   (eval-after-load "gnus"
;;     '(with-library org-contacts
;;        (setq org-contacts-files
;;              '("~/doc/org/contacts.org.gpg")
;;              org-contacts-icon-use-gravatar nil) ; yuck
;;        (org-contacts-gnus-insinuate)
;;        (define-key gnus-summary-mode-map (kbd "C-;")
;;          (lambda () (interactive) (org-capture nil "c"))))))

;; In case I accidentally my newsrc.eld
(eval-after-load "gnus-group"
  '(progn
     (setq gnus-topic-topology
           '(("Gnus" visible nil nil)
             (("rkm.id.au" visible nil ((gcc-self . "rkm.id.au/Sent"))))
             (("internode.on.net" visible nil
               ((gcc-self . "internode.on.net/Sent"))))
             (("flinders.edu.au" visible nil
               ((gcc-self . "flinders.edu.au/Sent"))))
             (("gmail.com" visible nil ((gcc-self . "gmail.com/Sent"))))
             (("huttriverprovince.com.au" visible nil
               ((gcc-self . "huttriverprovince.com.au/Sent"))))
             (("mangochutney.com.au" visible nil
               ((gcc-self . "mangochutney.com.au/Sent")))))
           gnus-topic-alist
           '(("Gnus" "archive" "nndraft:drafts")
             ("nnimap:flinders.edu.au"
              "nnimap:flinders.edu.au/Bin"
              "nnimap:flinders.edu.au/Keep"
              "nnimap:flinders.edu.au/New"
              "nnimap:flinders.edu.au/Sent"
              "nnimap:flinders.edu.au/Spam")
             ("nnimap:gmail.com"
              "nnimap:gmail.com/Bin"
              "nnimap:gmail.com/New"
              "nnimap:gmail.com/Spam"
              "nnimap:gmail.com/Sent")
             ("nnimap:huttriverprovince.com.au"
              "nnimap:huttriverprovince.com.au/New"
              "nnimap:huttriverprovince.com.au/Sent"
              "nnimap:huttriverprovince.com.au/Spam"
              "nnimap:huttriverprovince.com.au/Trash")
             ("nnimap:internode.on.net"
              "nnimap:internode.on.net/Bin"
              "nnimap:internode.on.net/Drafts"
              "nnimap:internode.on.net/New"
              "nnimap:internode.on.net/Sent"
              "nnimap:internode.on.net/Spam")
             ("nnimap:rkm.id.au"
              "nnimap:rkm.id.au/Keep"
              "nnimap:rkm.id.au/New"
              "nnimap:rkm.id.au/RSS"
              "nnimap:rkm.id.au/Sent"
              "nnimap:rkm.id.au/Spam"
              "nnimap:rkm.id.au/sa-learn"
              "nnimap:rkm.id.au/sa-unlearn")))))

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
