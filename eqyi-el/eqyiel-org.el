;;; eqyiel-org.el

(require 'org)

(setq org-directory "~/doc/org/")
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; http://julien.danjou.info/projects/emacs-packages#org-contacts

(with-library org-contacts
              (setq org-contacts-files '("~/doc/org/contacts.org.gpg"))
              (setq org-contacts-icon-use-gravatar nil) ;; yuck!
              (org-contacts-gnus-insinuate))

;; show me today +7 days in the agenda instead of this week, we don't care what
;; happened yesterday
(setq org-agenda-start-on-weekday nil)

(define-key gnus-summary-mode-map (kbd "C-;")
  (lambda () (interactive) (org-capture nil "c")))

(setq org-drawers (quote ("importants")))
;; (setq org-log-done (quote time))
(setq org-log-done 'time)
(setq org-agenda-files (file-expand-wildcards "~/doc/org/*.org.gpg"))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/doc/org/todo.org.gpg" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/doc/org/todo.org.gpg")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Contacts" entry (file "~/doc/org/contacts.org.gpg")
               "** %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:")
        ("m" "Add a contact manually" entry (file "~/doc/org/contacts.org.gpg")
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
