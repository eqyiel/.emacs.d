;;; eqyiel-org.el

(require 'eqyiel-ox-rkm-html)

(eval-after-load "eqyiel-ox-rkm-html"
  '(let ((rkm.id.au-base-directory "~/doc/rkm.id.au/src/")
         (rkm.id.au-publishing-directory "~/doc/rkm.id.au/pub/")
         (rkm.id.au-link-home "https://rkm.id.au/")
         (rkm.id.au-link-rss "https://rkm.id.au/rss/")
         (rkm.id.au-title "rkm.id.au")
         (rkm.id.au-description
          "Tech recipes and talking smack about free software.")
         (rkm.id.au-lang "en")
         (rkm.id.au-email user-mail-address)
         (rkm.id.au-author user-full-name)
         (rkm.id.au-html-head-template
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/head.html"))
         (rkm.id.au-preamble-template
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/preamble.html"))
         (rkm.id.au-analytics-script
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/analytics.html"))
         (rkm.id.au-extra-scripts '("/js/bootstrap.min.js")))
     (setq org-publish-project-alist
           `(("rkm.id.au"
              :components ("rkm.id.au-posts" "rkm.id.au-static" "rkm.id.au-base"
                           "rkm.id.au-css" "rkm.id.au-js" "rkm.id.au-favicon"
                           "rkm.id.au-robots"))
             ("rkm.id.au-posts"
              :base-directory
              ,(concat (file-name-directory rkm.id.au-base-directory) "posts/")
              :base-extension "org"
              :completion-function ox-rkm-html-posts-completion-function
              :description ,rkm.id.au-description
              :html-head ,rkm.id.au-html-head-template
              :html-link-home ,rkm.id.au-link-home
              :html-postamble ox-rkm-html-postamble
              :html-preamble ,rkm.id.au-preamble-template
              :preparation-function ox-rkm-html-posts-preparation-function
              :publishing-directory ,rkm.id.au-publishing-directory
              :publishing-function ox-rkm-html-publish-to-rkm-html
              :recursive t
              :rkm-html-extra-scripts ,rkm.id.au-extra-scripts
              :rkm-html-rss-email ,rkm.id.au-email
              :rkm-html-rss-link ,rkm.id.au-link-rss
              :rkm-html-rss-title ,rkm.id.au-title
              :rkm-html-analytics-script ,rkm.id.au-analytics-script)
             ("rkm.id.au-base"
              :base-directory
              ,(concat (file-name-directory rkm.id.au-base-directory) "base/")
              :base-extension "org"
              :description ,rkm.id.au-description
              :html-head ,rkm.id.au-html-head-template
              :html-link-home ,rkm.id.au-link-home
              :html-postamble nil
              :html-preamble ,rkm.id.au-preamble-template
              :publishing-directory ,rkm.id.au-publishing-directory
              :publishing-function ox-rkm-html-publish-base
              :recursive t
              :rkm-html-extra-scripts ,rkm.id.au-extra-scripts
              :rkm-html-analytics-script ,rkm.id.au-analytics-script)
             ("rkm.id.au-static"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "static/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "static/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-css"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "css/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "css/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-js"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "js/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "js/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-favicon"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "favicon/dist/")
              :base-extension any
              :publishing-directory
              ,(file-name-directory rkm.id.au-publishing-directory)
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-robots"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "robots/")
              :base-extension any
              :publishing-directory
              ,(file-name-directory rkm.id.au-publishing-directory)
              :publishing-function org-publish-attachment
              :recursive t)))))

(eval-after-load "org-mobile"
  '(setq org-mobile-directory "~/owncloud/org"
         org-mobile-inbox-for-pull "~/doc/org/from-mobile.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (emacs-lisp . t)
   (sh . t)))

(eval-after-load "org"
  '(progn
     (setq
      org-image-actual-width '(400)
      org-directory "~/doc/org"
      org-log-done 'time
      org-agenda-files (file-expand-wildcards "~/doc/org/*.org*")
      org-descriptive-links nil
      org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      org-confirm-babel-evaluate nil
      org-export-babel-evaluate nil
      org-export-default-language "en"
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
:END:")))
     (add-hook 'org-mode-hook 'turn-on-auto-fill)))

(when (string-equal (system-name) "ayanami")
  (eval-after-load "gnus"
    '(with-library org-contacts
       (setq org-contacts-files
             '("~/doc/org/contacts.org.gpg")
             org-contacts-icon-use-gravatar nil) ; yuck
       (org-contacts-gnus-insinuate)
       (define-key gnus-summary-mode-map (kbd "C-;")
         (lambda () (interactive) (org-capture nil "c"))))))

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

(autoload 'org-caldav-sync "org-caldav" nil t)

(eval-after-load "org-caldav"
  '(setq org-caldav-save-directory "~/.cache/emacs" ; don't litter
         org-caldav-url
         "http://eigengrau.rkm.id.au/remote.php/caldav/calendars/eqyiel"
         org-caldav-calendar-id "test" ; name of calendar in owncloud
         org-caldav-inbox "~/doc/org/caldav-test.org"
         org-caldav-files
         '("~/doc/org/test-calendar-events.org")
         org-icalendar-timezone "Australia/Adelaide"))


(defun eqyiel-org-clock-sum-today ()
  "Visit each file in `org-agenda-files' and return the total time of today's
clocked tasks in minutes."
  (let ((files (org-agenda-files))
        (total 0))
    (org-agenda-prepare-buffers files)
    (dolist (file files)
      (with-current-buffer (find-buffer-visiting file)
        (setq total (+ total (org-clock-sum-today)))))
    total))


(defun eqyiel-org-select-src-lang-mode ()
  "Select a language's key from the alist of languages org-mode groks."
  (interactive)
  (insert
   (completing-read
    "Select language: "
    '(asymptote awk C C++ clojure css d ditaa dot calc
                emacs-lisp fortran gnuplot haskell java
                js latex ledger lisp lilypond matlab
                mscgen ocaml octave org oz perl plantuml
                processing python R ruby sass scheme
                screen sed sh sql sqlite))))

(use-package org-download
  :config (setq org-download-method 'attach
                ;; doesn't work on OSX :(
                ;; org-download-timestamp t
                ))

(provide 'eqyiel-org)
