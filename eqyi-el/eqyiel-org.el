;;; eqyiel-org.el

(eval-after-load "org"
  '(progn
     (setq
      org-directory "/ssh:eigenlicht:/home/eqyiel/org"
      org-log-done 'time
      org-agenda-files (file-expand-wildcards "/ssh:eigenlicht:/home/eqyiel/org/*.org*")
      org-capture-templates
      '(("t" "Todo" entry
         (file+headline "/ssh:eigenlicht:/home/eqyiel/org/new.org.gpg" "Tasks")
         "* TODO %?")
        ("r" "Read" entry
         (file+headline "/ssh:eigenlicht:/home/eqyiel/org/new.org.gpg" "Read")
         "** %?")
        ("w" "Watch" entry
         (file+headline "/ssh:eigenlicht:/home/eqyiel/org/new.org.gpg" "Watch")
         "** %?")
        ("l" "Listen" entry
         (file+headline "/ssh:eigenlicht:/home/eqyiel/org/new.org.gpg" "Listen")
         "** %?")
        ("c" "Contacts" entry
         (file "/ssh:eigenlicht:/home/eqyiel/org/contacts.org.gpg")
         "** %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:")
        ("m" "Add a contact manually" entry
         (file "/ssh:eigenlicht:/home/eqyiel/org/contacts.org.gpg")
         "** %^{Name}
  :PROPERTIES:
  :EMAIL:
  :END:"))
      org-todo-keywords ; pinched from sachac
      '((sequence
         "TODO(t)"  ; next action
         "TOBLOG(b)"  ; next action
         "STARTED(s)"
         "WAITING(w@/!)"
         "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "TODELEGATE(-)" "DELEGATED(d)" "COMPLETE(x)")))
     (add-hook 'org-mode-hook 'turn-on-auto-fill)))

(eval-after-load "gnus"
  '(with-library org-contacts
     (setq org-contacts-files
           '("/ssh:eigenlicht:/home/eqyiel/org/contacts.org.gpg")
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
         org-caldav-inbox "/ssh:eigenlicht:/home/eqyiel/org/caldav-test.org"
         org-caldav-files
         '("/ssh:eigenlicht:/home/eqyiel/org/test-calendar-events.org")
         org-icalendar-timezone "Australia/Adelaide"))

(provide 'eqyiel-org)
