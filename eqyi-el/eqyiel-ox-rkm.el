;;; ox-rkm.el

(require 'org)
(require 'ox-publish)
(require 'ox-org)
(require 'ox-html)
(require 'ox-rss)

(require 'cl)
(require 'dash)
(require 'subr-x)

;; refreshing the DB works nicely now, see if we can just use the org-publish
;; api to store data instead of relying on a secondary db.
;; another advantage of this would be that ox-rkm-html-plists and ox-rkm-html-db
;; would become unecessary.
;; use ox-rkm-publish-cache-set and org-combine-plists instead
;; http://orgmode.org/worg/sources/org-api/org-publish-api.org
;; for rss and stuff, can use something like this, but modify the sort to sort
;; by (plist-get ... :date) instead (if it exists, not all items in
;; org-publish-cache will have :date)

;; look at the white-space css property and the pre-line attribute to handle
;; indentation of source code blocks, try to replace every indent with &nbsp;
;; https://stackoverflow.com/questions/17257200/how-to-stop-indenting-html-source-creating-whitespace-in-pre-code-tags?lq=1

(defun hash-table-values (hash-table)
  (let ((values ()))
    (maphash
     (lambda (k v)
       (let ((plist (gethash k hash-table)))
         (when (and (plist-get plist :rkm-date)
                    (not (member "noexport" (plist-get plist :rkm-filetags))))
         (push v values)))) hash-table)
    values))
;; (hash-table-values org-publish-cache)
;; (subseq
;;  (sort (hash-table-keys org-publish-cache)
;;        (lambda (x y)
;;          (if (time-less-p (plist-get x :rkm-date)
;;                           (plist-get y :rkm-date)) nil t))) 0 1)
;; could do similarly for creating feeds by tag

;; for debugging:
;; (org-publish-reset-cache)

;; TODO: this should not need to be global, make it so that it can be enabled
;; per-project.
;; (setq org-export-htmlize-output-type 'css)

;; To see whether a file needs publishing, first initialize the cache.  It will
;; be loaded from disk.  `org-publish-cache' only holds one project's cache at a
;; time.
;; (org-publish-initialize-cache "rkm.id.au-posts")

;; Then call `org-publish-cache-file-needs-publishing':
;; (org-publish-cache-file-needs-publishing
;;  (expand-file-name
;;   "~/doc/rkm.id.au/src/posts/test.org")
;;  (expand-file-name rkm-publishing-directory)
;;  'org-rkm-html-publish-to-rkm-html)
;; (ox-rkm-touch-file "~/doc/rkm.id.au/src/posts/test.org")

;; (remhash (org-publish-timestamp-filename
;;  (expand-file-name
;;   "~/doc/rkm.id.au/src/posts/ui-shitstorm.org")
;;  (expand-file-name rkm-publishing-directory)
;;  'org-rkm-html-publish-to-rkm-html) org-publish-cache)

;; (defun go-publish ()
;;   (let ((org-html-htmlize-output-type 'css)
;;         (htmlize-ignore-face-size t))
;;     (advice-add 'org-export-output-file-name :filter-return
;;                 'ox-rkm-export-output-file-name)
;;     (benchmark-run 1 (org-rkm-html-refresh-db))
;;     ;; (benchmark-run 0 (org-publish-all t nil))
;;     (benchmark-run 0 (org-publish-all))
;;     (advice-remove 'org-export-output-file-name
;;                    'ox-rkm-export-output-file-name)
;;     (org-rkm-make-rss)))

;; (go-publish)

;; (benchmark-run 0 (org-publish-all))
;; (benchmark-run 0 (org-publish "rkm.id.au-posts"))

(defvar ox-rkm-sources ())
(defvar ox-rkm-format-time-string "%s") ;; epoch time
(defvar org-rkm-html-plists ()
  "A list of plists with org keyword attributes.")
(defvar org-rkm-html-db ()
  "A hash table, key is the filename sans extension.  Value is the plist
representing that file.")

(defun ox-rkm--posts-preparation-function ()
  (let ((org-html-htmlize-output-type 'css)
        (htmlize-ignore-face-size t))
    ;; this is done automatically
    ;; (org-publish-initialize-cache "rkm.id.au-posts")
    ;; TODO: pass as a value to org-rkm-html-refresh-db, better than using setq and defvar
    (setq ox-rkm-sources (org-publish-get-base-files
                          (rassoc project-plist org-publish-project-alist)))
    (advice-add 'org-export-output-file-name :filter-return
                'ox-rkm-export-output-file-name)
    (benchmark-run 1 (org-rkm-html-refresh-db))))

(defun ox-rkm--posts-completion-function ()
    (advice-remove 'org-export-output-file-name
                   'ox-rkm-export-output-file-name))

;; Adapted from here:
;; https://emacs.stackexchange.com/questions/17714/publish-org-mode-to-html-using-a-separate-directory-for-each-post
;; Maybe make a follow up post for anyone else who wants to use the new advice
;; system instead (easier to enable and disable per project, show demonstration
;; of preparation-function and completion-function)
(defun ox-rkm-export-output-file-name (output-file)
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
         (dir-name (file-name-sans-extension
                    (file-name-nondirectory visited-file)))
         (extension ".html"))
    ;; (message "ox-rkm-export-to-directories: visited-file: %s" visited-file)
    ;; (message "ox-rkm-export-to-directories: dir-name: %s" dir-name)
    (replace-regexp-in-string
     (regexp-quote (concat dir-name extension))
     (concat "index" extension)
     output-file)))

(defun org-rkm-make-rss ()
  (dolist (entry (delq nil (subseq org-rkm-html-plists 0 10)))
    (let* ((src (plist-get entry :rkm-src))
           (author (plist-get entry :author))
           (title (plist-get entry :title))
           (permalink (plist-get entry :html))
           (pubdate (let ((system-time-locale "C"))
                      (format-time-string
                       "%a, %d %b %Y %H:%M:%S %z"
                       (plist-get entry :date))))
           (org-inhibit-startup t)
           (visitingp (find-buffer-visiting src))
           (work-buffer (or visitingp (find-file-noselect src))))
      (with-current-buffer work-buffer
        (setf entry
              (plist-put entry :fragment
                         (format
                          (concat
                           "<item>\n"
                           "<title>%s</title>\n"
                           "<link>%s</link>\n"
                           "<author>%s (%s)</author>\n"
                           "<guid>%s</guid>\n"
                           "<pubDate>%s</pubDate>\n"
                           "<description><![CDATA[%s]]></description>\n"
                           "</item>\n")
                          title permalink rkm-rss-email author permalink pubdate
                          (string-trim (org-export-as 'html nil t t))))))))
  (let* ((system-time-locale "C")
         (date (format-time-string "%a, %d %b %Y %H:%M:%S %z")))
    (with-temp-buffer
      (insert
       (concat
        (format "<?xml version=\"1.0\" encoding=\"%s\"?>"
                (symbol-name org-html-coding-system))
        "\n<rss version=\"2.0\"
  xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
  xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"
  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
  xmlns:atom=\"http://www.w3.org/2005/Atom\"
  xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\"
  xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\"
  xmlns:georss=\"http://www.georss.org/georss\"
        xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\"
        xmlns:media=\"http://search.yahoo.com/mrss/\">"
        "<channel>")
       (format
        "\n<title>%s</title>
<atom:link href=\"%s\" rel=\"self\" type=\"application/rss+xml\" />
<link>%s</link>
<description>%s</description>
<language>%s</language>
<pubDate>%s</pubDate>
<lastBuildDate>%s</lastBuildDate>
<generator>%s</generator>
"
        rkm-rss-title rkm-rss-link rkm-link-home rkm-rss-description
        rkm-rss-lang date date
        (concat (format "Emacs %d.%d"
                        emacs-major-version
                        emacs-minor-version)
                " Org-mode " (org-version))))
      (dolist (entry (delq nil (subseq org-rkm-html-plists 0 10)))
        (insert (plist-get entry :fragment)))
      (insert (concat "</channel>\n" "</rss>"))
      (xml-mode)
      (indent-region (point-min) (point-max))
      (write-file (concat
                   (file-name-as-directory
                    (expand-file-name rkm-publishing-directory))
                   "rss")))))

(defun org-rkm-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'.

Modified to use the correct path to output files if `(file-name-base raw-path)'
was a key in `org-rkm-html-db'."
  (let* ((home (when (plist-get info :html-link-home)
                 (org-trim (plist-get info :html-link-home))))
         (use-abs-url (plist-get info :html-link-use-abs-url))
         (link-org-files-as-html-maybe
          (function
           (lambda (raw-path info)
             "Treat links to `file.org' as links to `file.html', if needed.
           See `org-html-link-org-files-as-html'."
             (cond
              ((and (plist-get info :html-link-org-files-as-html)
                    (string= ".org"
                             (downcase (file-name-extension raw-path "."))))
               (replace-regexp-in-string
                (regexp-quote (expand-file-name rkm-publishing-directory)) ""
                (plist-get
                 ;; (gethash (file-name-base raw-path) org-rkm-html-db
                 ;;          `(:rkm-html
                 ;;            ,(concat (file-name-sans-extension raw-path) "."
                 ;;                     (plist-get info :html-extension))))
                 (org-publish-cache-get (expand-file-name raw-path))
                 :rkm-html)))
              (t raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
          (cond
           ((member type '("http" "https" "ftp" "mailto"))
            (org-html-encode-plain-text
             (org-link-escape-browser
              (org-link-unescape (concat type ":" raw-path)))))
           ((string= type "file")
            ;; Treat links to ".org" files as ".html", if needed.
            (setq raw-path
                  (funcall link-org-files-as-html-maybe raw-path info))
            ;; If file path is absolute, prepend it with protocol
            ;; component - "file://".
            (cond
             ((file-name-absolute-p raw-path)
              (setq raw-path (org-export-file-uri raw-path)))
             ((and home use-abs-url)
              (setq raw-path (concat (file-name-as-directory home) raw-path))))
            ;; Add search option, if any.  A search option can be
            ;; relative to a custom-id, a headline title a name,
            ;; a target or a radio-target.
            (let ((option (org-element-property :search-option link)))
              (if (not option) raw-path
                (concat raw-path
                        "#"
                        (org-publish-resolve-external-link
                         option
                         (org-element-property :path link))))))
           (t raw-path)))
         ;; Extract attributes from parent's paragraph.  HACK: Only do
         ;; this for the first link in parent (inner image link for
         ;; inline images).  This is needed as long as attributes
         ;; cannot be set on a per link basis.
         (attributes-plist
          (let* ((parent (org-export-get-parent-element link))
                 (link (let ((container (org-export-get-parent link)))
                         (if (and (eq (org-element-type container) 'link)
                                  (org-html-inline-image-p link info))
                             container
                           link))))
            (and (eq (org-element-map parent 'link 'identity info t) link)
                 (org-export-read-attribute :attr_html parent))))
         (attributes
          (let ((attr (org-html--make-attribute-string attributes-plist)))
            (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p
            link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "<a href=\"#%s\"%s>%s</a>"
                  (org-export-get-reference destination info)
                  attributes
                  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (case (org-element-type destination)
          ;; ID link points to an external file.
          (plain-text
           (let ((fragment (concat "ID-" path))
                 ;; Treat links to ".org" files as ".html", if needed.
                 (path (funcall link-org-files-as-html-maybe
                                destination info)))
             (format "<a href=\"%s#%s\"%s>%s</a>"
                     path fragment attributes (or desc destination))))
          ;; Fuzzy link points nowhere.
          ((nil)
           (format "<i>%s</i>"
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; Link points to a headline.
          (headline
           (let ((href (or (org-element-property :CUSTOM_ID destination)
                           (org-export-get-reference destination info)))
                 ;; What description to use?
                 (desc
                  ;; Case 1: Headline is numbered and LINK has no
                  ;; description.  Display section number.
                  (if (and (org-export-numbered-headline-p destination info)
                           (not desc))
                      (mapconcat #'number-to-string
                                 (org-export-get-headline-number
                                  destination info) ".")
                    ;; Case 2: Either the headline is un-numbered or
                    ;; LINK has a custom description.  Display LINK's
                    ;; description or headline's title.
                    (or desc
                        (org-export-data
                         (org-element-property :title destination) info)))))
             (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
          ;; Fuzzy link points to a target or an element.
          (t
           (let* ((ref (org-export-get-reference destination info))
                  (org-html-standalone-image-predicate
                   #'org-html--has-caption-p)
                  (number (cond
                           (desc nil)
                           ((org-html-standalone-image-p destination info)
                            (org-export-get-ordinal
                             (org-element-map destination 'link
                               #'identity info t)
                             info 'link 'org-html-standalone-image-p))
                           (t (org-export-get-ordinal
                               destination info nil 'org-html--has-caption-p))))
                  (desc (cond (desc)
                              ((not number) "No description for this link")
                              ((numberp number) (number-to-string number))
                              (t (mapconcat #'number-to-string number ".")))))
             (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
        (format "<a href=\"#%s\"%s%s>%s</a>"
                fragment
                (org-trim
                 (format (concat "class=\"coderef\""
                                 " onmouseover=\"CodeHighlightOn(this, '%s');\""
                                 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
                         fragment fragment))
                attributes
                (format (org-export-get-coderef-format path desc)
                        (org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

;; Export to HTML with some extra keywords.
(org-export-define-derived-backend 'rkm-html 'html
  :translate-alist '((link . org-rkm-html-link))
  :options-alist
  '((:rkm-filetags "RKM_FILETAGS" nil nil t)
    (:rkm-link-tags "RKM_LINK_TAGS" nil nil t)
    (:rkm-html-next "RKM_NEXT" nil nil t)
    (:rkm-html "RKM_HTML" nil nil t)
    (:rkm-html-prev "RKM_PREV" nil nil t)))

(defun org-rkm-get-publication-dir (filename date)
  "DATE is a time object, like what is returned by current-time."
  (file-name-as-directory
   (concat
    (expand-file-name
     rkm-publishing-directory)
    (format-time-string
     "%Y/%m/%d/" date)
    (file-name-base filename))))

;; TODO: this needs rewriting now that I'm saving results to org-publish-cache
;; and not rkm-html-db
(defun org-rkm-html-publish-to-rkm-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* (;; (data (gethash (file-name-base filename) org-rkm-html-db))
         (data (org-publish-cache-get (expand-file-name filename)))
         (real-pub-dir (plist-get data :rkm-html)))
    (message "data: %S" data)
    (when (and ;; (or
                (or (org-publish-cache-file-needs-publishing
                    (expand-file-name filename)
                    (expand-file-name (plist-get plist :publishing-directory))
                    (plist-get plist :publishing-function))
                    (not (file-exists-p (concat real-pub-dir "index.html"))))
               (not (member "noexport" (plist-get data :rkm-filetags))))
      (message "%s needs publishing, doing it." filename)
      (org-publish-org-to
       'rkm-html filename
       (concat
        "." (or (plist-get plist :html-extension)
                org-html-extension "html"))
       (org-combine-plists
        plist '(:publishing-directory real-pub-dir)) real-pub-dir))))

(defun org-rkm-postamble (info)
  "Return the postamble as a string built from INFO.  INFO is a plist of export
options."
  (let ((data (org-publish-cache-get
               (expand-file-name (plist-get info :input-buffer)))
              ;; (gethash (expand-file-name (plist-get info :input-buffer))
              ;;          org-publish-cache)
              )
        (rkm-next (plist-get data :rkm-html-next))
        (rkm-prev (plist-get data :rkm-html-prev)))
    ;; ((tags (plist-get info :tags))
        ;; ;; (rkm-html (plist-get info :rkm-html))
        ;; (rkm-next (plist-get info :rkm-html-next))
        ;; (rkm-prev (plist-get info :rkm-html-prev)))
    (message "working on %s" (file-name-base (plist-get data :rkm-src)))
    (message "next is %s" rkm-next)
    (message "prev is %s" rkm-prev)
    (concat
     ;; (when (and (stringp rkm-html) (not (string-empty-p rkm-html)))
     ;;   (format "<p class=\"permalink\">%s</p>" rkm-html))
     ;; (when (and (and tags (listp tags))
     ;;            (and (stringp rkm-link-tags)
     ;;                 (not (string-empty-p rkm-link-tags))))
     ;;   (format
     ;;    "<p class=\"tags\">Tagged: %s</p>\n"
     ;;    (mapconcat
     ;;     (function
     ;;      (lambda (tag)
     ;;        (format "<a href=\"%s%s\" title=\"Posts tagged with %s\">%s</a>"
     ;;                rkm-link-tags tag tag tag))) tags " ")))
     (when (and (stringp rkm-next) (not (string-empty-p rkm-next)))
       (format "<p class =\"next\">%s</p>\n"
               (format "<a href=\"%s\" title=\"Next entry\">Next</a>"
                       (concat
                        "/"
                        (string-remove-prefix
                        (file-name-as-directory
                         (expand-file-name rkm-publishing-directory)) rkm-next)))))
     (when (and (stringp rkm-prev) (not (string-empty-p rkm-prev)))
       (format "<p class =\"prev\">%s</p>\n"
               (format "<a href=\"%s\" title=\"Previous entry\">Previous</a>"
                       (concat
                        "/"
                        (string-remove-prefix
                        (file-name-as-directory
                         (expand-file-name rkm-publishing-directory)) rkm-prev))))))))

(defun ox-rkm-find-property (env key)
  (let ((value (plist-get env key)))
    (cond ((equal :date key)
           (org-time-string-to-time (org-element-interpret-data value)))
          ((or (equal :author key) (equal :title key))
           (org-no-properties (org-element-interpret-data value)))
          (t value))))

(defun ox-rkm-get-parent-directory (dir)
  (file-name-directory (directory-file-name dir)))

(defun ox-rkm-directory-empty-p (dir)
  "True if DIR has no more than two items (\".\" \"..\")."
  (and (file-exists-p dir)
       (> 3 (length (directory-files dir)))))

(defun ox-rkm-delete-empty-directories (dir)
  (when (ox-rkm-directory-empty-p dir)
    (delete-directory dir t t)
    (ox-rkm-delete-empty-directories
     (ox-rkm-get-parent-directory dir))))

(defun ox-rkm-touch-file (file)
  (let* ((org-inhibit-startup t)
           (visitingp (find-buffer-visiting (expand-file-name file)))
           (work-buffer (or visitingp (find-file-noselect (expand-file-name file)))))
      (with-current-buffer work-buffer
        (when (and (verify-visited-file-modtime (current-buffer))
                 (not (buffer-modified-p)))
          (set-buffer-modified-p t)
          (save-buffer 0)))))

(defun org-rkm-html-refresh-db ()
  ;; (setq org-rkm-html-plists '())
  ;; (setq org-rkm-html-db (make-hash-table :test 'equal))
  (let ((plists '()))
  (dolist (src ox-rkm-sources) ;; create a "database" of posts
    (let* ((org-inhibit-startup t)
           (visitingp (find-buffer-visiting src))
           (work-buffer (or visitingp (find-file-noselect src))))
      (with-current-buffer work-buffer
        (let* ((env (org-export-get-environment))
               (author (ox-rkm-find-property env :author))
               (date (ox-rkm-find-property env :date))
               (title (ox-rkm-find-property env :title))
               (filetags (ox-rkm-find-property env :filetags))
               (dest (org-rkm-get-publication-dir src date)))
          (push () plists)
          (setf (car plists) (plist-put (car plists) :rkm-author author))
          (setf (car plists) (plist-put (car plists) :rkm-date date))
          (setf (car plists) (plist-put (car plists) :rkm-title title))
          (setf (car plists) (plist-put (car plists) :rkm-src src))
          (setf (car plists) (plist-put (car plists) :rkm-html dest))
          (setf (car plists) (plist-put (car plists) :rkm-filetags filetags))
          (if (member "noexport" filetags)
              ;; Clean up if necessary
              (progn
                (when (file-exists-p (concat dest "index.html"))
                  (delete-file (concat dest "index.html") t)
                  (ox-rkm-delete-empty-directories dest))))))))
  (setq plists ;; sort descending by date, most recent first
        (sort plists
              (lambda (x y)
                (if (time-less-p (plist-get x :rkm-date)
                                 (plist-get y :rkm-date)) nil t))))
  ;; find each previous item
  (dolist (plist plists)
    (let* ((prev-element
            (ox-rkm-find-next
             (cadr
              (-split-at
               (+ 1 (-elem-index plist plists))
               plists)))))
      (when prev-element
        (let ((prev-known-rkm-html-prev
               (ox-rkm-publish-cache-get-property
                (plist-get plist :rkm-src) :rkm-html-prev))
              (rkm-html-prev (plist-get prev-element :rkm-html)))
          (when (not (string-equal prev-known-rkm-html-prev rkm-html-prev))
            (message "prev known rkm prev not equal to prev.")
              (ox-rkm-touch-file (plist-get plist :rkm-src)))
          (setf plist (plist-put plist :rkm-html-prev rkm-html-prev))))))
  ;; find each next item
  (let ((plists (reverse plists)))
    (dolist (plist plists)
      (let ((next-element
             (ox-rkm-find-next
              (cadr (-split-at (+ 1 (-elem-index plist plists)) plists))))
            (rkm-src (plist-get plist :rkm-src)))
        (when next-element
          (let ((prev-known-rkm-html-next
                 (ox-rkm-publish-cache-get-property rkm-src :rkm-html-next))
                (rkm-html-next (plist-get next-element :rkm-html)))
            (when (not (string-equal prev-known-rkm-html-next rkm-html-next))
              (message "prev known rkm next not equal to next.")
              (ox-rkm-touch-file rkm-src))
            (setf plist (plist-put plist :rkm-html-next rkm-html-next))))
        (ox-rkm-publish-cache-combine-plists rkm-src plist))))
  (org-publish-write-cache-file)
  plists))

(defun ox-rkm-find-next (plists)
  "Get the next plist in the list of PLISTS that does not have member noexport
in its filetags.  Return the plist or nil if there is no next element."
  (let* ((plist (car plists))
         (prev-src (plist-get plist :rkm-html))
         (noexportp (member "noexport" (plist-get plist :rkm-filetags))))
    (if (not noexportp) plist (ox-rkm-find-next (cdr plists)))))

;; (ox-rkm-publish-cache-set-property "~/doc/rkm.id.au/src/posts/test.org" :lol "hi")
(defun ox-rkm-publish-cache-set-property (file-name prop value)
  (let ((plist (org-publish-cache-get (expand-file-name file-name))))
    (org-publish-cache-set plist (plist-put plist prop value))))

;; (ox-rkm-publish-cache-get-property "~/doc/rkm.id.au/src/posts/test.org" :lol)
(defun ox-rkm-publish-cache-get-property (file-name prop)
  (org-publish-cache-get-file-property (expand-file-name file-name) prop))

;; (ox-rkm-publish-cache-combine-plists
;;  "~/doc/rkm.id.au/src/posts/test.org" '(:yolo "no way"))
(defun ox-rkm-publish-cache-combine-plists (file-name plist)
  "Combine PLIST with the plist retrieved from key in `org-publish-cache' and
store it in `org-publish-cache'."
  (let ((key (expand-file-name file-name)))
    (org-publish-cache-set
     key
     (org-combine-plists
      (org-publish-cache-get key) plist))))

(provide 'ox-rkm)
