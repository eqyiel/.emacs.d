;;; eqyiel-org.el

(eval-after-load "org-mobile"
  '(setq org-mobile-directory "~/owncloud/org"
         org-mobile-inbox-for-pull "~/doc/org/from-mobile.org"))


(defvar rkm.id.au-html-head
    "<link href='/static/favicon.png' rel='shortcut icon' type='image/x-icon'>
<link rel='stylesheet' href='/static/css/style.css' type='text/css'/>"

;;   "<link href='/static/favicon.png' rel='shortcut icon' type='image/x-icon'>
;; <link rel='stylesheet' href='css/site.css?v=2' type='text/css'/>
;; <meta name='viewport' content='width=device-width, initial-scale=1'>
;; <script src='/js/Hyphenator.js' type='text/javascript'></script>
;; <script src='/js/jquery.js' type='text/javascript'></script>
;; <link href='http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css' rel='stylesheet'>"
  )

;; (defvar rkm.id.au-html-preamble
;;   "<div class='intro'>
;; <h1><b>Nicolas</b> Petton</h1>
;; <p>Software engineer, Smalltalker & Emacs Lisper. Also loves the Web.</p>
;; </div>
;; <div class='nav'>
;; <ul>
;; <li><a href='/'>Home</a>/</li>
;; <li><a href='/blog/index.html'>Blog</a>/</li>
;; <li><a href='http://github.com/NicolasPetton'>GitHub</a>/</li>
;; <li><a href='http://twitter.com/NicolasPetton'>Twitter</a>/</li>
;; <li><a href='/contact.html'>Contact</a></li>
;; </ul>
;; </div>
;; ")

(defvar rkm-link-home "https://rkm.id.au/")

(defvar rkm-link-tags (concat rkm-link-home "tags/")
  "See `rkm-html-postamble'.  Should be nil or a string.")

(defvar rkm-rss-link "https://rkm.id.au/rss")
(defvar rkm-rss-title "rkm.id.au")
(defvar rkm-rss-description "Tech recipes and talking smack.")
(defvar rkm-rss-lang "en")
(defvar rkm-rss-email user-mail-address)
(defvar rkm-rss-author user-full-name)
(defvar rkm-rss-image)
(defvar rkm.id.au-posts-directory "~/doc/rkm.id.au/src/posts")
(defvar rkm.id.au-src-static "~/doc/rkm.id.au/src/static")
(defvar rkm.id.au-pub-static "~/doc/rkm.id.au/pub/static")
(defvar rkm.id.au-base-directory "~/doc/rkm.id.au/src/base")
(defvar rkm-publishing-directory "~/doc/rkm.id.au/pub/")
(setq org-publish-use-timestamps-flag nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (emacs-lisp . t)
   (sh . t)))

(eval-after-load "org"
  '(setq org-publish-project-alist
         `(("rkm.id.au"
            :components ("rkm.id.au-posts" "rkm.id.au-static" ;; "rkm.id.au-rss"
                         ))
        ("rkm.id.au-posts"
         :auto-sitemap nil
         :auto-index nil
         ;; :index-filename "index.org"
         ;; :index-title "rkm.id.au"
         :base-directory ,rkm.id.au-posts-directory
         :base-extension "org"
         :publishing-directory ,rkm-publishing-directory
         :recursive t
         :preparation-function ox-rkm--posts-preparation-function
         :completion-function ox-rkm--posts-completion-function
         :publishing-function org-rkm-html-publish-to-rkm-html
         :export-with-tags nil ;; t
         :headline-levels 4
         :section-numbers nil
         :sub-superscript nil
         :todo-keywords nil
         :author ,user-full-name
         :creator-info nil
         :section-numbers nil
         :with-toc nil
         ;; for only publishing <body>...</body>
         ;; :body-only t
         :html-head ,rkm.id.au-html-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-use-infojs nil
         :html-indent nil ;; breaks src blocks
         :html-preamble nil
         ;; :html-preamble ,rkm.id.au-html-preamble
         :html-postamble org-rkm-postamble
         :html-html5-fancy t
         :html-doctype "html5"
         :html-link-up ,rkm-link-home
         :html-link-home "/" ;; ,rkm-link-home
         :html-home/up-format ""
         ;; :html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"
         :rkm-link-tags ,rkm-link-tags
         :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />";; "This is raw html for stylesheet <link>'s"
         :timestamp t
         :exclude-tags ("noexport" "todo")
         :auto-preamble t)
        ("rkm.id.au-base"
         :auto-sitemap nil
         :auto-index nil
         ;; :index-filename "index.org"
         ;; :index-title "rkm.id.au"
         :base-directory ,rkm.id.au-base-directory
         :base-extension "org"
         :publishing-directory ,rkm-publishing-directory
         :recursive t
         :preparation-function org-rkm-html-refresh-db
         ;; :completion-function ox-rkm-symlink-index
         :publishing-function org-rkm-html-publish-to-rkm-html
         :export-with-tags nil
         :headline-levels 4
         :section-numbers nil
         :sub-superscript nil
         :todo-keywords nil
         :author ,user-full-name
         :creator-info nil
         :section-numbers nil
         :with-toc nil
         ;; for only publishing <body>...</body>
         ;; :body-only t
         :html-head ,rkm.id.au-html-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-use-infojs nil
         :html-indent nil ;; breaks src blocks
         ;; :html-preamble ,rkm.id.au-html-preamble
         :html-preamble nil
         :html-postamble org-rkm-postamble
         :html-html5-fancy t
         :html-doctype "html5"
         :html-link-up ,rkm-link-home
         :html-link-home "/" ;; ,rkm-link-home
         :html-home/up-format ""
         :rkm-link-tags ,rkm-link-tags
         :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />";; "This is raw html for stylesheet <link>'s"
         :timestamp t
         :exclude-tags ("noexport" "todo")
         :auto-preamble t)
        ("rkm.id.au-static"
         :base-directory ,rkm.id.au-src-static
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|otf"
         :publishing-directory ,rkm.id.au-pub-static
         :recursive t
         :publishing-function org-publish-attachment))))

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
  (insert (completing-read "Select language: "
                           '(asymptote awk C C++ clojure css d ditaa dot calc
                                       emacs-lisp fortran gnuplot haskell java
                                       js latex ledger lisp lilypond matlab
                                       mscgen ocaml octave org oz perl plantuml
                                       processing python R ruby sass scheme
                                       screen sed sh sql sqlite)
                           ;; `org-src-lang-modes' doesn't list all of the
                           ;; supported languages >__>
                           ;; ()(mapcar 'car org-src-lang-modes)
                           )))

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
