;;; eqyiel-org.el

(eval-after-load "org"
  '(progn
     (setq
      org-directory "~/doc/org"
      org-log-done 'time
      org-agenda-files (file-expand-wildcards "~/doc/org/*.org*")
      org-capture-templates
      '(("t" "Todo" entry (file+headline "~/doc/org/new.org" "Tasks")
         "* TODO %?")
        ("r" "Read" entry (file+headline "~/doc/org/new.org" "Read")
         "** %?")
        ("w" "Watch" entry (file+headline "~/doc/org/new.org" "Watch")
         "** %?")
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
     (add-hook 'org-mode-hook 'turn-on-auto-fill)))

(eval-after-load "gnus"
  '(with-library org-contacts
     (setq org-contacts-files '("~/doc/org/contacts.org.gpg")
           org-contacts-icon-use-gravatar nil) ; yuck
     (org-contacts-gnus-insinuate)
     (define-key gnus-summary-mode-map (kbd "C-;")
       (lambda () (interactive) (org-capture nil "c")))))

(global-set-key (kbd "<f12>") 'org-capture)

(defun eqyiel-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; adapted from
;; http://stackoverflow.com/questions/6681407/org-mode-capture-with-sexp
(defun org-insert-link-with-page-title (url)
  "Given URL, retrieve its title element and do org-insert-link at point with
URL and title."
  (interactive "sURL: ")
  (insert
   (with-current-buffer (url-retrieve-synchronously url)
     (goto-char (point-min))
     (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
     (setq title (match-string 1))
     (goto-char (point-min))
     (if (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
         (setq charset (downcase (match-string 1)))
       (setq charset "utf-8")
       (setq title (decode-coding-string title (intern charset))))
     (erase-buffer)
     (org-insert-link nil url title)
     (buffer-string))))

;; why does this not give me nice utf-8 characters?
;; (org-insert-link-with-page-title
;; "https://github.com/jorgenschaefer/circe/wiki/Configuration")

(autoload 'org-caldav-sync "org-caldav" nil t)

;; If I end up using this a lot I should probably add the details to authinfo
;; machine www.google.com:443 port https login username password secret

(eval-after-load "org-caldav"
  '(setq org-caldav-save-directory "~/.cache/emacs" ; don't litter
         org-caldav-url
         "http://eigengrau.rkm.id.au/remote.php/caldav/calendars/eqyiel"
         org-caldav-calendar-id "test" ; name of calendar in owncloud
         org-caldav-inbox "~/doc/org/caldav-test.org"
         org-caldav-files '("~/doc/org/test-calendar-events.org")
         org-icalendar-timezone "Australia/Adelaide"))

(provide 'eqyiel-org)
