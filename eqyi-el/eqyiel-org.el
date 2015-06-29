;;; eqyiel-org.el

(eval-after-load "org-mobile"
  '(setq org-mobile-directory "~/owncloud/org"
         org-mobile-inbox-for-pull "~/doc/org/from-mobile.org"))

(eval-after-load "org"
  '(progn
     (setq
      org-directory "~/doc/org"
      org-log-done 'time
      org-agenda-files (file-expand-wildcards "~/doc/org/*.org*")
      org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/doc/org/new.org.gpg" "Tasks")
         "** TODO %?")
        ("r" "Read" entry
         (file+headline "~/doc/org/new.org.gpg" "Read")
         "** %?")
        ("w" "Watch" entry
         (file+headline "~/doc/org/new.org.gpg" "Watch")
         "** %?")
        ("l" "Listen" entry
         (file+headline "~/doc/org/new.org.gpg" "Listen")
         "** %?")
        ("u" "Quote" entry
         (file+headline "~/doc/org/new.org.gpg" "Quote")
         "** %?")
        ("c" "Contacts" entry
                   (file "~/doc/org/contacts.org.gpg")
                   "** %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
                  ("m" "Add a contact manually" entry
                   (file "~/doc/org/contacts.org.gpg")
                   "** %^{Name}
:PROPERTIES:
:EMAIL:
:END:")
        )
      org-publish-project-alist
      '(("rkm.id.au"
         :components ("rkm.id.au-content" "rkm.id.au-static" "rkm.id.au-rss"))
        ("rkm.id.au-content"
         :auto-sitemap t
         :auto-index t
         :index-filename "index.org"
         :index-title "rkm.id.au"
         :base-directory "~/doc/blog/src/"
         :base-extension "org"
         :publishing-directory "~/doc/blog/pub/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :export-with-tags nil
         :headline-levels 4
         :toc nil
         :section-numbers nil
         :sub-superscript nil
         :todo-keywords nil
         :author nil
         :creator-info nil
         ;; :html-preamble "rkm.id.au"
         ;; :html-postamble nil
         :html-html5-fancy t
         :html-doctype "html5"
         :html-link-up "https://rkm.id.au/"
         :html-link-home "https://rkm.id.au/"
         :style "This is raw html for stylesheet <link>'s"
         :timestamp t
         :exclude-tags ("noexport" "todo")
         :auto-preamble t)
        ("rkm.id.au-static"
         :base-directory "~/doc/blog/src"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|otf"
         :publishing-directory "~/doc/blog/pub/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("rkm.id.au-rss"
         :base-directory "~/doc/blog/src/"
         :base-extension "org"
         ;; :rss-image-url "http://lumiere.ens.fr/~guerry/images/faces/15.png"
         ;; :html-link-home "http://lumiere.ens.fr/~guerry/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :publishing-directory "~/doc/blog/pub/"
         :publishing-function (org-rss-publish-to-rss)
         :section-numbers nil
         :exclude ".*"            ;; To exclude all files...
         :include ("sitemap.org")   ;; ... except index.org.
         :table-of-contents nil)))
     (add-hook 'org-mode-hook 'turn-on-auto-fill)))

;; (eval-after-load "org-contacts"
;;   '(add-to-list org-capture-templates
;;                 '()))

(eval-after-load "gnus"
  '(with-library org-contacts
     (setq org-contacts-files
           '("~/doc/org/contacts.org.gpg")
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
         org-caldav-files
         '("~/doc/org/test-calendar-events.org")
         org-icalendar-timezone "Australia/Adelaide"))

;; insert timestamps when an heading is created

(add-hook 'org-insert-heading-hook
          'tj/insert-heading-inactive-timestamp 'append)

(defvar tj/insert-inactive-timestamp t)

(defun tj/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq tj/insert-inactive-timestamp
        (not tj/insert-inactive-timestamp))
  (message "Heading timestamps are %s"
           (if tj/insert-inactive-timestamp "ON" "OFF")))

(defun tj/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun tj/insert-heading-inactive-timestamp ()
  (save-excursion
    (when tj/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (tj/insert-inactive-timestamp))))

;; switch to calfw buffer at startup
(defun eqyiel-init-hook ()
  (with-library org-contacts
    (eqyiel-open-calendar)
    (get-buffer-create "*cfw-calendar*")
    ;; (setq initial-buffer-choice "*cfw-calendar*") just opens a new file
    ))

(add-hook 'after-init-hook
          'eqyiel-init-hook)

(provide 'eqyiel-org)
