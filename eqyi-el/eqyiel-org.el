;;; eqyiel-org.el

(require 'org)

(setq org-directory "~/doc/org/")
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(require 'org-contacts)

;; http://julien.danjou.info/projects/emacs-packages#org-contacts
(setq org-contacts-files '("~/doc/org/contacts.org"))
(setq org-contacts-icon-use-gravatar nil) ;; yuck!

;;; Make it easy to use in Gnus.
(org-contacts-gnus-insinuate)
(define-key gnus-summary-mode-map (kbd "C-;")
  (lambda () (interactive) (org-capture nil "c")))

(setq org-drawers (quote ("importants")))
;; (setq org-log-done (quote time))
(setq org-log-done 'time)
(setq org-agenda-files (file-expand-wildcards "~/doc/org/*.org"))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/doc/org/orgtest.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/doc/org/orgtest.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Contacts" entry (file "~/doc/org/contacts.org")
               "** %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:")
        ("m" "Add a contact manually" entry (file "~/doc/org/contacts.org")
         "** %^{Name}
  :PROPERTIES:
  :EMAIL:
  :END:")))

(add-hook 'org-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (yas-minor-mode -1)))

;; cool when it works, pain in the ass when it doesn't
;; (org-indent-mode t)

(provide 'eqyiel-org)
