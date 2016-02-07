;;; eqyiel-ox-rkm-html.el

(require 'org)
(require 'ox-publish)
(require 'ox-org)
(require 'ox-html)
(require 'ox-rss)

(require 'cl)
(require 'dash)
(require 'subr-x)

;; still to do:
;; show date/time underneath title?

(defgroup eqyiel-ox-rkm-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Derived backend for rkm.id.au"
  :group 'eqyiel-ox-rkm-html)

(defcustom ox-rkm-html-rss-title ""
  "Title that should be used for the generated RSS."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-rss-link ""
  "URL pointing to the RSS feed."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-rss-entry-template "<item>
<title>%s</title>
<link>%s</link>
<author>%s (%s)</author>
<guid>%s</guid>
<pubDate>%s</pubDate>
<description>
<![CDATA[
%s
]]>
</description>
</item>\n"
  "Raw XML to be used to build each RSS item.  It should contain seven instances
of %s, which will be replaced by the title, permalink, author, email,
GUID, publication date and the entry itself respectively."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-rss-item-template "<title>%s</title>
<atom:link href=\"%s\" rel=\"self\" type=\"application/rss+xml\" />
<link>%s</link>
<description>%s</description>
<language>%s</language>
<pubDate>%s</pubDate>
<lastBuildDate>%s</lastBuildDate>
<generator>%s</generator>\n"
  "Raw XML to be used to build RSS metadata.  It should contain eight instances
of %s, which will be replaced by the title, rss link, link home, description,
language, pubdate, last build date and generator respectively."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-rss-head-template "<rss version=\"2.0\"
       xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
       xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"
       xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
       xmlns:atom=\"http://www.w3.org/2005/Atom\"
       xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\"
       xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\"
       xmlns:georss=\"http://www.georss.org/georss\"
       xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\"
       xmlns:media=\"http://search.yahoo.com/mrss/\">\n"
  "Raw XML to be inserted in the RSS header."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-preamble-template ""
  "Raw HTML to be inserted in the preamble."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-html-head-template ""
  "Raw HTML to be inserted in the head."
  :group 'eqyiel-ox-rkm-html
  :type 'string)

(defcustom ox-rkm-html-extra-scripts '()
  "A list of paths to extra scripts that should be appended to the end of the
  body.  Be sure to use a form that the server will recognise, they will not
  automatically be copied to the publication directory."
  :group 'eqyiel-ox-rkm-html
  :type 'list)

(defvar ox-rkm-html-publishing-directory ""
  "There is no easy way to access this from some internal functions
(cf. `ox-rkm-html-headline'), so this is set from the value passed in
`org-publish-project-alist'.  You probably don't want to set it manually.")

(defun ox-rkm-html-string-from-file (file)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name file))
    (buffer-string)))

(defun ox-rkm-html-get-hash-table-values (hash-table &optional tag)
  (let ((values ()))
    (maphash
     (lambda (k v)
       (let ((plist (gethash k hash-table)))
         (when (and (plist-get plist :rkm-date)
                    (not (member "noexport" (plist-get plist :rkm-filetags)))
                    (if tag (member tag (plist-get plist :rkm-filetags)) t))
           (push v values)))) hash-table)
    values))

(defun ox-rkm-html-posts-preparation-function ()
  "`project-plist' is scoped automagically into this context."
  (advice-add 'org-export-output-file-name :filter-return
              'ox-rkm-html-export-file-name)
  (setq ox-rkm-html-publishing-directory
        (file-name-directory
         (expand-file-name
         (plist-get project-plist :publishing-directory))))
  ;; (advice-add 'org-html-fontify-code :around 'ox-rkm-html-fontify-code)
  (benchmark-run 1 (ox-rkm-html-refresh-db
                    (org-publish-get-base-files
                     (rassoc project-plist org-publish-project-alist))
                    project-plist)))

(defun ox-rkm-html-posts-completion-function ()
  "`project-plist' is scoped automagically into this context."
  (let ((plists (ox-rkm-html-sort-by-date
                 (ox-rkm-html-get-hash-table-values org-publish-cache))))
    (org-publish-write-cache-file)
    (ox-rkm-html-generate-rss plists project-plist)
    ;; (advice-remove 'org-html-fontify-code 'ox-rkm-html-fontify-code)
    (advice-remove 'org-export-output-file-name
                   'ox-rkm-html-export-file-name)
    (ox-rkm-html-generate-archive plists project-plist)
    (let* ((index (concat ox-rkm-html-publishing-directory "index.html")))
      (when (file-symlink-p index) (delete-file index))
      (shell-command
       (concat "ln -s "
               (replace-regexp-in-string
                ox-rkm-html-publishing-directory
                "" (plist-get (car plists) :rkm-html))
               "index.html" " " index)))))

(defun ox-rkm-html-get-all-tags ()
  (let ((all-tags ()))
    (dolist (plist (ox-rkm-html-get-hash-table-values org-publish-cache))
      (mapcar
       (lambda (arg) (push arg all-tags))
       (plist-get plist :rkm-filetags)))
    all-tags))

(defun ox-rkm-html-export-file-name (output-file)
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
         (dir-name (file-name-sans-extension
                    (file-name-nondirectory visited-file)))
         (extension ".html"))
    (replace-regexp-in-string
     (regexp-quote (concat dir-name extension))
     (concat "index" extension)
     output-file)))

(defun ox-rkm-html-generate-archive (plists project-plist)
  (let* ((output-file (concat ox-rkm-html-publishing-directory "archive/index.html"))
        (target-directory (concat ox-rkm-html-publishing-directory "archive")))
    (with-temp-buffer
    (insert "#+title: Archive\n")
    (insert (format "#+date: %s\n"
                    (format-time-string "<%Y-%m-%d %a %R>" (current-time))))
    (insert (mapconcat
             (lambda (arg)
               (concat
                "- =" (format-time-string "%Y-%m-%d" (plist-get arg :rkm-date))
                "= [[" (expand-file-name (plist-get arg :rkm-src))
                "][" (plist-get arg :rkm-title) "]]\n"))
             plists ""))
    (unless (file-exists-p target-directory)
      (make-directory target-directory))
    (org-export-to-file
        'rkm-html
        output-file
      nil nil nil nil
      (org-combine-plists
       project-plist
       `(:html-postamble nil
         :rkm-html-with-date nil
         :output-file ,output-file))))))

(defun ox-rkm-html-generate-rss (plists project-plist &optional tag)
  (let* ((target-directory (concat ox-rkm-html-publishing-directory "/rss"))
         (system-time-locale "C")
         (date (format-time-string "%a, %d %b %Y %H:%M:%S %z"))
         (rss-title (plist-get project-plist :rkm-html-rss-title))
         (rss-link (plist-get project-plist :rkm-html-rss-link))
         (rss-description (plist-get project-plist :description))
         (link-home (plist-get project-plist :html-link-home))
         (language (or (plist-get project-plist :language)
                       org-export-default-language))
         (rss-email (plist-get project-plist :rkm-html-rss-email))
         (org-html-htmlize-output-type 'css)
         (org-inhibit-startup t)
         (output-file
          (concat target-directory
                  (if tag
                      (concat "/" tag ".rss")
                    "/index.rss"))))
        (dolist (entry (delq nil (subseq plists 0 10)))
      (let* ((src (plist-get entry :rkm-src))
             (author (plist-get entry :rkm-author))
             (title (plist-get entry :rkm-title))
             (rkm-html (plist-get entry :rkm-html))
             (permalink (concat (directory-file-name link-home)
                                (ox-rkm-html-get-permalink rkm-html)))
             (pubdate (let ((system-time-locale "C"))
                        (format-time-string
                         "%a, %d %b %Y %H:%M:%S %z"
                         (plist-get entry :rkm-date))))
             (visitingp (find-buffer-visiting src))
             (work-buffer (or visitingp (find-file-noselect src))))
        (with-current-buffer work-buffer
          (setf entry
                (plist-put entry :fragment
                           (format
                            ox-rkm-html-rss-entry-template
                            title permalink rss-email author permalink
                            pubdate
                            (string-trim
                             (org-export-as
                              'rkm-html nil t t
                              `(:output-file ,rkm-html
                                :rkm-html-with-date nil)))))))))
    (with-temp-buffer
      (insert
       (concat
        (format "<?xml version=\"1.0\" encoding=\"%s\"?>"
                (symbol-name org-html-coding-system))
        ox-rkm-html-rss-head-template)
       "<channel>\n"
       (format
        ox-rkm-html-rss-item-template
        rss-title rss-link link-home rss-description language date date
        (concat (format "Emacs %d.%d"
                        emacs-major-version
                        emacs-minor-version)
                " Org-mode " (org-version))))
      (dolist (entry (delq nil (subseq plists 0 10)))
        (insert (plist-get entry :fragment)))
      (insert (concat "</channel>\n" "</rss>"))
      (xml-mode)
      ;; breaks code indentation
      ;; (indent-region (point-min) (point-max))
      (when (file-exists-p target-directory)
        (delete-directory target-directory t nil))
      (make-directory target-directory)
      (write-file output-file))))

(defun ox-rkm-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home ;; (when (plist-get info :html-link-home)
               ;;   (org-trim (plist-get info :html-link-home)))
          "/"
               )
         (use-abs-url (plist-get info :html-link-use-abs-url))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (plist (org-publish-cache-get (expand-file-name raw-path)))
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
                (regexp-quote  ox-rkm-html-publishing-directory) ""
                (plist-get plist :rkm-html)))
              (t raw-path)))))
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

(defun ox-rkm-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
   (lambda (str)
     (replace-regexp-in-string
      "\"" "&quot;" (org-html-encode-plain-text str))))
  (title (org-export-data (plist-get info :title) info))
  (author (and (plist-get info :with-author)
         (let ((auth (plist-get info :author)))
           (and auth
          ;; Return raw Org syntax, skipping non
          ;; exportable objects.
          (org-element-interpret-data
           (org-element-map auth
         (cons 'plain-text org-element-all-objects)
             'identity info))))))
  (description (plist-get info :description))
  (keywords (plist-get info :keywords))
  (charset (or (and org-html-coding-system
        (fboundp 'coding-system-get)
        (coding-system-get org-html-coding-system
               'mime-charset))
         "iso-8859-1")))
    (concat
     (when (plist-get info :time-stamp-file)
       (format-time-string
  (concat "<!-- "
    (plist-get info :html-metadata-timestamp-format)
    " -->\n")))
     (format
      (if (org-html-html5-p info)
    (org-html-close-tag "meta" "charset=\"%s\"" info)
  (org-html-close-tag
   "meta" "http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
   info))
      charset) "\n"
     (let ((viewport-options
      (org-remove-if-not (lambda (cell) (org-string-nw-p (cadr cell)))
             (plist-get info :html-viewport))))
       (and viewport-options
      (concat
       (org-html-close-tag
        "meta"
        (format "name=\"viewport\" content=\"%s\""
          (mapconcat
           (lambda (elm) (format "%s=%s" (car elm) (cadr elm)))
           viewport-options ", "))
        info)
       "\n")))
     (format "<title>%s</title>\n" title)
     (org-html-close-tag "meta" "name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (and (org-string-nw-p author)
    (concat
     (org-html-close-tag "meta"
             (format "name=\"author\" content=\"%s\""
               (funcall protect-string author))
             info)
     "\n"))
     (and (org-string-nw-p description)
    (concat
     (org-html-close-tag "meta"
             (format "name=\"description\" content=\"%s\""
               (funcall protect-string description))
             info)
     "\n"))
     (and (org-string-nw-p keywords)
    (concat
     (org-html-close-tag "meta"
             (format "name=\"keywords\" content=\"%s\""
               (funcall protect-string keywords))
             info)
     "\n")))))

(defun ox-rkm-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
            (decl (or (and (stringp xml-declaration) xml-declaration)
                      (cdr (assoc (plist-get info :html-extension)
                                  xml-declaration))
                      (cdr (assoc "html" xml-declaration))
                      "")))
       (when (not (or (not decl) (string= "" decl)))
         (format "%s\n"
                 (format decl
                         (or (and org-html-coding-system
                                  (fboundp 'coding-system-get)
                                  (coding-system-get org-html-coding-system 'mime-charset))
                             "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
           (cond ((org-html-xhtml-p info)
                  (format
                   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                   (plist-get info :language) (plist-get info :language)))
                 ((org-html-html5-p info)
                  (format " lang=\"%s\"" (plist-get info :language))))
           ">\n")
   "<head>\n"
   (ox-rkm-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
         (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
               (or link-up link-home)
               (or link-home link-up))))
   ;; Preamble.
   (ox-rkm-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n" (nth 1 div) (nth 2 div) (nth 3 div)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let* ((title (plist-get info :title))
            (subtitle
             (if (plist-get info :rkm-html-with-date)
                 (format-time-string "<%Y-%m-%d %a %R>"
                                     (org-time-string-to-time
                                      (org-element-interpret-data
                                       (plist-get info :date))))
               nil))
           (html5-fancy (org-html--html5-fancy-p info))
           (output-file (plist-get info :output-file))
           (permalink
            (if output-file (ox-rkm-html-get-permalink output-file) nil)))
       (when title
         (format
          (if html5-fancy
              "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
            "<h1 class=\"title\">%s%s</h1>\n")
          (if permalink
              (format "%s\n<a class=\"permalink\" href=\"%s\">∞</a>"
                      (org-export-data title info) permalink)
            (org-export-data title info))
          (if subtitle
              (format
               (if html5-fancy
                   "<p class=\"subtitle\">%s</p>\n"
                 "\n<br>\n<span class=\"subtitle\">%s</span>\n")
               (if permalink
                   (format "<a class=\"permalink\" href=\"%s\">%s</a>"
                           permalink (org-export-data subtitle info))
                   (org-export-data subtitle info)))
            "")))
       ;; (when title
       ;;   (format
       ;;    (if html5-fancy
       ;;        "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
       ;;      "<h1 class=\"title\">%s%s</h1>\n")
       ;;    (let* ((output-file (plist-get info :output-file)))
       ;;      (if output-file
       ;;          (format "%s\n<a class=\"permalink\" href=\"%s\">∞</a>"
       ;;                  (org-export-data title info)
       ;;                  (ox-rkm-html-get-permalink output-file))
       ;;        (org-export-data title info)))
       ;;    (if subtitle
       ;;        (format
       ;;         (if html5-fancy
       ;;             "<p class=\"subtitle\">%s</p>\n"
       ;;           "\n<br>\n<span class=\"subtitle\">%s</span>\n")
       ;;         (org-export-data subtitle info))
       ;;      "")))
       ))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (ox-rkm-html--build-pre/postamble 'postamble info)
   (let ((extra-scripts (plist-get info :rkm-html-extra-scripts)))
     (when extra-scripts (mapconcat
    (lambda (arg)
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n" arg))
    extra-scripts "")))
   (let ((analytics-script (plist-get info :rkm-html-analytics-script)))
     (when analytics-script analytics-script))
   ;; Closing document.
   "</body>\n</html>"))

(defun ox-rkm-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
     (ids (delq nil
                      (list (org-element-property :CUSTOM_ID headline)
                            (org-export-get-reference headline info)
                            (org-element-property :ID headline))))
           (preferred-id (car ids))
           (extra-ids
      (mapconcat
       (lambda (id)
         (org-html--anchor
    (if (org-uuidgen-p id) (concat "ID-" id) id)
    nil nil info))
       (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((type (if numberedp 'ordered 'unordered))
                 (itemized-body
                  (org-html-format-list-item
                   contents type nil info nil
                   (concat (org-html--anchor preferred-id nil nil info)
                           extra-ids
                           full-text))))
            (concat (and (org-export-first-sibling-p headline info)
                         (org-html-begin-plain-list type))
                    itemized-body
                    (and (org-export-last-sibling-p headline info)
                         (org-html-end-plain-list type))))
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          ;; Standard headline.  Export it as a section.
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (concat "outline-container-"
        (org-export-get-reference headline info))
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                          level
                          preferred-id
                          extra-ids
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (mapconcat #'number-to-string numbers ".")))
                           (format
                            "%s\n<a class=\"permalink\" href=\"%s#%s\">∞</a>"
                            full-text (ox-rkm-html-get-permalink (plist-get info :output-file)) preferred-id))
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(defun ox-rkm-html-get-permalink (output-file)
  (concat
   "/"
   (string-remove-prefix
    ox-rkm-html-publishing-directory
    (string-remove-suffix "index.html" output-file))))

(defun ox-rkm-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (ox-rkm-html-footnote-section info)))

(defun ox-rkm-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (loop for (n _type raw) in fn-alist collect
                (cons n (if (eq (org-element-type raw) 'org-data)
                            (org-trim (org-export-data raw info))
                          (format "<aside class=\"footpara\">%s</aside>"
                                  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (format
       "<hr>\n<div id=\"footnotes\">\n<ol>\n%s</ol>\n</div>\n"
       (format
        "\%s"
        (mapconcat
         (lambda (fn)
           (let ((n (car fn)) (def (cdr fn)))
             (format
              "<li class=\"footdef\">%s</li>"
              (replace-regexp-in-string
               "</p></aside>$"
               (concat
                (org-html--anchor
                 (format "fn.%d" n)
                 "↩"
                 (format " class=\"footnum\" href=\"%s#fnr.%d\""
                         (ox-rkm-html-get-permalink
                          (plist-get info :output-file)) n)
                 info) "</p></aside>")
               def)
              )))
         fn-alist
         "\n"))))))

(defun ox-rkm-html-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (id (format "fnr.%d%s"
                      n
                      (if (org-export-footnote-first-reference-p
                           footnote-reference info)
                          ""
                        ".100"))))
     (format
      (plist-get info :html-footnote-format)
      (org-html--anchor
       id n
       (format
        " class=\"footref\" href=\"%s#fn.%d\""
        (ox-rkm-html-get-permalink
         (plist-get info :output-file)) n)
       info)))))

(defun ox-rkm-html--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil.
TYPE is either `preamble' or `postamble', INFO is a plist used as a
communication channel."
  (let ((section (plist-get info (intern (format ":html-%s" type))))
        (spec (org-html-format-spec info)))
    (when section
      (let ((section-contents
             (if (functionp section) (funcall section info)
               (cond
                ((stringp section) (format-spec section spec))
                ((eq section 'auto)
                 (let ((date (cdr (assq ?d spec)))
                       (author (cdr (assq ?a spec)))
                       (email (cdr (assq ?e spec)))
                       (creator (cdr (assq ?c spec)))
                       (timestamp (cdr (assq ?T spec)))
                       (validation-link (cdr (assq ?v spec))))
                   (concat
                    (when (and (plist-get info :with-date)
                               (org-string-nw-p date))
                      (format "<p class=\"date\">%s: %s</p>\n"
                              (org-html--translate "Date" info)
                              date))
                    (when (and (plist-get info :with-author)
                               (org-string-nw-p author))
                      (format "<p class=\"author\">%s: %s</p>\n"
                              (org-html--translate "Author" info)
                              author))
                    (when (and (plist-get info :with-email)
                               (org-string-nw-p email))
                      (format "<p class=\"email\">%s: %s</p>\n"
                              (org-html--translate "Email" info)
                              email))
                    (when (plist-get info :time-stamp-file)
                      (format
                       "<p class=\"date\">%s: %s</p>\n"
                       (org-html--translate "Created" info)
                       (format-time-string
                        (plist-get info :html-metadata-timestamp-format))))
                    (when (plist-get info :with-creator)
                      (format "<p class=\"creator\">%s</p>\n" creator))
                    (format "<p class=\"validation\">%s</p>\n"
                            validation-link))))
                (t (format-spec
                    (or (cadr (assoc
                               (plist-get info :language)
                               (eval (intern
                                      (format "org-html-%s-format" type)))))
                        (cadr
                         (assoc
                          "en"
                          (eval
                           (intern (format "org-html-%s-format" type))))))
                    spec))))))
        (let ((div (assq type (plist-get info :html-divs))))
          (when (org-string-nw-p section-contents)
            (concat
             (format
              (concat "<%s id=\"%s\""
                      (if (>= (length div) 4) " class=\"%s\">\n" ">\n"))
              (nth 1 div)
              (nth 2 div)
              ;; org-html--pre/postamble-class
              (when (>= (length div) 4) (nth 3 div)))
             (org-element-normalize-string section-contents)
             (format "</%s>\n" (nth 1 div)))))))))

(defun ox-rkm-html-get-publication-dir (filename date)
  "DATE is a time object, like what is returned by current-time."
  (file-name-as-directory
   (concat
    ox-rkm-html-publishing-directory
    (format-time-string
     "%Y/%m/%d/" date)
    (file-name-base filename))))

(defun ox-rkm-html-publish-base (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((org-html-postamble nil))
    (org-publish-org-to
     'rkm-html filename
     (concat "." (or (plist-get plist :html-extension)
                     org-html-extension
                     "html"))
     (org-combine-plists
      plist `(:publishing-directory ,(plist-get plist :publishing-directory)
              :rkm-html-with-date nil))
     pub-dir)))

;; (defun ox-rkm-html-export-as-rkm-html
;;   (&optional async subtreep visible-only body-only ext-plist)
;;   "Export current buffer to an HTML buffer.

;; If narrowing is active in the current buffer, only export its
;; narrowed part.

;; If a region is active, export that region.

;; A non-nil optional argument ASYNC means the process should happen
;; asynchronously.  The resulting buffer should be accessible
;; through the `org-export-stack' interface.

;; When optional argument SUBTREEP is non-nil, export the sub-tree
;; at point, extracting information from the headline properties
;; first.

;; When optional argument VISIBLE-ONLY is non-nil, don't export
;; contents of hidden elements.

;; When optional argument BODY-ONLY is non-nil, only write code
;; between \"<body>\" and \"</body>\" tags.

;; EXT-PLIST, when provided, is a property list with external
;; parameters overriding Org default settings, but still inferior to
;; file-local settings.

;; Export is done in a buffer named \"*Org HTML Export*\", which
;; will be displayed when `org-export-show-temporary-export-buffer'
;; is non-nil."
;;   (interactive)
;;   (org-export-to-buffer 'rkm-html "*Org HTML Export*"
;;     async subtreep visible-only body-only ext-plist
;;     (lambda () (set-auto-mode t))))

(defun ox-rkm-html-publish-to-rkm-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* ((data (org-publish-cache-get (expand-file-name filename)))
         (real-pub-dir (plist-get data :rkm-html))
         (org-html-htmlize-output-type 'css))
    (when (and
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
        plist `(:publishing-directory ,real-pub-dir)) real-pub-dir))))

(defun ox-rkm-html-postamble (info)
  "Return the postamble as a string built from INFO.  INFO is a plist of export
options."
  (let ((data (org-publish-cache-get
               (expand-file-name (plist-get info :input-buffer))))
        (rkm-next (plist-get data :rkm-html-next))
        (rkm-prev (plist-get data :rkm-html-prev)))
    (concat
     "<nav>\n<ul class=\"pager\">\n"
     (when (and (stringp rkm-prev) (not (string-empty-p rkm-prev)))
       (format "<li class=\"previous\">%s</li>\n"
               (format "<a href=\"/%s\"><span aria-hidden=\"true\">&larr;</span>Older</a>"
                       (string-remove-prefix ox-rkm-html-publishing-directory rkm-prev))))
     (when (and (stringp rkm-next) (not (string-empty-p rkm-next)))
       (format "<li class=\"next\">%s</li>\n"
               (format "<a href=\"/%s\">Newer<span aria-hidden=\"true\">&rarr;</span></a>"
                       (string-remove-prefix ox-rkm-html-publishing-directory rkm-next))))
     "</ul>\n</nav>")))

(defun ox-rkm-html-find-property (env key)
  (let ((value (plist-get env key)))
    (cond ((equal :date key)
           (org-time-string-to-time (org-element-interpret-data value)))
          ((or (equal :author key) (equal :title key))
           (org-no-properties (org-element-interpret-data value)))
          (t value))))

(defun ox-rkm-html-get-parent-directory (dir)
  (file-name-directory (directory-file-name dir)))

(defun ox-rkm-html-directory-empty-p (dir)
  "True if DIR has no more than two items (\".\" \"..\")."
  (and (file-exists-p dir)
       (> 3 (length (directory-files dir)))))

(defun ox-rkm-html-delete-empty-directories (dir)
  (when (ox-rkm-html-directory-empty-p dir)
    (delete-directory dir t t)
    (ox-rkm-html-delete-empty-directories
     (ox-rkm-html-get-parent-directory dir))))

(defun ox-rkm-html-touch-file (file)
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting (expand-file-name file)))
         (work-buffer (or visitingp (find-file-noselect (expand-file-name file)))))
    (with-current-buffer work-buffer
      (when (and (verify-visited-file-modtime (current-buffer))
                 (not (buffer-modified-p)))
        (set-buffer-modified-p t)
        (save-buffer 0)))))

(defun ox-rkm-html-refresh-db (sources project-plist)
  (let ((plists '()))
    (dolist (src sources) ;; create a "database" of posts
      (let* ((org-inhibit-startup t)
             (visitingp (find-buffer-visiting src))
             (work-buffer (or visitingp (find-file-noselect src))))
        (with-current-buffer work-buffer
          (let* ((env (org-export-get-environment))
                 (author (ox-rkm-html-find-property env :author))
                 (date (ox-rkm-html-find-property env :date))
                 (title (ox-rkm-html-find-property env :title))
                 (filetags (ox-rkm-html-find-property env :filetags))
                 (dest (ox-rkm-html-get-publication-dir src date)))
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
                    (ox-rkm-html-delete-empty-directories dest))))))))
    (setq plists (ox-rkm-html-sort-by-date plists))
    ;; find each previous item
    (dolist (plist plists)
      (let* ((prev-element
              (ox-rkm-html-find-next
               (cadr
                (-split-at
                 (+ 1 (-elem-index plist plists))
                 plists)))))
        (when prev-element
          (let ((prev-known-rkm-html-prev
                 (ox-rkm-html-publish-cache-get-property
                  (plist-get plist :rkm-src) :rkm-html-prev))
                (rkm-html-prev (plist-get prev-element :rkm-html)))
            (when (not (string-equal prev-known-rkm-html-prev rkm-html-prev))
              (ox-rkm-html-touch-file (plist-get plist :rkm-src)))
            (setf plist (plist-put plist :rkm-html-prev rkm-html-prev))))))
    ;; find each next item
    (let ((plists (reverse plists)))
      (dolist (plist plists)
        (let ((next-element
               (ox-rkm-html-find-next
                (cadr (-split-at (+ 1 (-elem-index plist plists)) plists))))
              (rkm-src (plist-get plist :rkm-src)))
          (when next-element
            (let ((prev-known-rkm-html-next
                   (ox-rkm-html-publish-cache-get-property rkm-src :rkm-html-next))
                  (rkm-html-next (plist-get next-element :rkm-html)))
              (when (not (string-equal prev-known-rkm-html-next rkm-html-next))
                (ox-rkm-html-touch-file rkm-src))
              (setf plist (plist-put plist :rkm-html-next rkm-html-next))))
          (ox-rkm-html-publish-cache-combine-plists rkm-src plist))))
    (org-publish-write-cache-file)
    plists))

(defun ox-rkm-html-find-next (plists)
  "Get the next plist in the list of PLISTS that does not have member noexport
in its filetags.  Return the plist or nil if there is no next element."
  (let* ((plist (car plists))
         (prev-src (plist-get plist :rkm-html))
         (noexportp (member "noexport" (plist-get plist :rkm-filetags))))
    (if (not noexportp) plist (ox-rkm-html-find-next (cdr plists)))))

(defun ox-rkm-html-publish-cache-set-property (file-name prop value)
  (let ((plist (org-publish-cache-get (expand-file-name file-name))))
    (org-publish-cache-set plist (plist-put plist prop value))))

(defun ox-rkm-html-publish-cache-get-property (file-name prop)
  (org-publish-cache-get-file-property (expand-file-name file-name) prop))

(defun ox-rkm-html-publish-cache-combine-plists (file-name plist)
  "Combine PLIST with the plist retrieved from key in `org-publish-cache' and
store it in `org-publish-cache'."
  (let ((key (expand-file-name file-name)))
    (org-publish-cache-set
     key
     (org-combine-plists
      (org-publish-cache-get key) plist))))

(defun ox-rkm-html-sort-by-date (plists)
  (sort plists
        (lambda (x y)
          (if (time-less-p (plist-get x :rkm-date)
                           (plist-get y :rkm-date)) nil t))))

;; (defun ox-rkm-html-fontify-code (fun code lang)
;;   "Hack to replace leading whitespace with non-breaking spaces.  Use this around
;; `org-html-fontify-code' if you want the exported HTML to be indented without
;; messing up indentation of source or example blocks."
;;   (replace-regexp-in-string
;;    (if (or
;;         (not (functionp
;;               (and (or (assoc-default lang org-src-lang-modes) lang)
;;                    (intern
;;                     (format
;;                      "%s-mode"
;;                      (or (assoc-default lang org-src-lang-modes) lang))))))
;;         ;; U+FFFC (OBJECT REPLACEMENT CHARACTER)
;;         (not lang)) "￼" "&#65532;") "&nbsp;"
;;         (funcall
;;          fun
;;          (with-temp-buffer
;;            (insert code)
;;            (goto-char (point-min))
;;            (ignore-errors
;;              (while (re-search-forward "^[ |\t]+")
;;                (replace-match
;;                 (replace-regexp-in-string "[ |\t]" "￼" (match-string 0)))))
;;            (buffer-string)) lang)))

;; Monkey patched to get rid of invalid face errors
;; (defun org-html-htmlize-generate-css ()
;;   "Create the CSS for all font definitions in the current Emacs session.
;; Use this to create face definitions in your CSS style file that can then
;; be used by code snippets transformed by htmlize.
;; This command just produces a buffer that contains class definitions for all
;; faces used in the current Emacs session.  You can copy and paste the ones you
;; need into your CSS file.

;; If you then set `org-html-htmlize-output-type' to `css', calls
;; to the function `org-html-htmlize-region-for-paste' will
;; produce code that uses these same face definitions."
;;   (interactive)
;;   (require 'htmlize)
;;   (and (get-buffer "*html*") (kill-buffer "*html*"))
;;   (with-temp-buffer
;;     (let ((fl (face-list))
;;     (htmlize-css-name-prefix "org-")
;;     (htmlize-output-type 'css)
;;     f i)
;;       (setq fl (delete 'message-header-from fl))
;;       (setq fl (delete 'gnus-header-from fl))
;;       (while (setq f (pop fl)
;;        i (and f (face-attribute f :inherit)))
;;   (when (and (symbolp f) (or (not i) (not (listp i))))
;;     (insert (org-add-props (copy-sequence "1") nil 'face f))))
;;       (htmlize-region (point-min) (point-max))))
;;   (org-pop-to-buffer-same-window "*html*")
;;   (goto-char (point-min))
;;   (if (re-search-forward "<style" nil t)
;;       (delete-region (point-min) (match-beginning 0)))
;;   (if (re-search-forward "</style>" nil t)
;;       (delete-region (1+ (match-end 0)) (point-max)))
;;   (beginning-of-line 1)
;;   (if (looking-at " +") (replace-match ""))
;;   (goto-char (point-min)))


(org-export-define-derived-backend 'rkm-html 'html
  :translate-alist
  '((footnote-reference . ox-rkm-html-footnote-reference)
    (inner-template . ox-rkm-html-inner-template)
    (link . ox-rkm-html-link)
    (template . ox-rkm-html-template)
    (headline . ox-rkm-html-headline))
  :options-alist
  ;; (:prop keyword option default behaviour)
  `((:section-numbers nil nil nil nil)
    (:with-toc "toc" nil nil nil)
    (:html-divs nil nil '((preamble  "nav" "preamble"
                                     "navbar navbar-rkm")
                          (content   "div" "content" "container")
                          (postamble "div" "postamble" "container")) nil)
    (:with-creator nil nil t nil)
    (:html-head-include-default-style "html-style" nil nil nil)
    (:html-head-include-scripts "html-scripts" nil nil nil)
    (:html-use-infojs nil nil nil nil)
    (:html-indent nil nil nil nil)
    (:html-head "HTML_HEAD" nil ox-rkm-html-html-head-template nil)
    (:html-preamble nil nil ox-rkm-html-preamble-template nil)
    (:html-link-home "HTML_LINK_HOME" nil "/" nil)
    (:html-link-up "HTML_LINK_UP" nil "/" nil)
    (:html-doctype "HTML_DOCTYPE" nil "html5" nil)
    (:html-html5-fancy "html5-fancy" nil t nil)
    (:html-link-use-abs-url "html-link-use-abs-url" nil t nil)
    (:html-home/up-format nil nil "" t)
    (:language nil nil "en" nil)
    (:rkm-html-analytics-script nil nil nil nil)
    (:rkm-html-extra-scripts nil nil ox-rkm-html-extra-scripts nil)
    (:rkm-html-rss-title nil nil ,ox-rkm-html-rss-title nil)
    (:rkm-html-rss-link nil nil ,ox-rkm-html-rss-link nil)
    (:rkm-html-rss-email nil nil ,user-mail-address nil)
    (:rkm-html-with-date nil nil t nil)))

(provide 'eqyiel-ox-rkm-html)
