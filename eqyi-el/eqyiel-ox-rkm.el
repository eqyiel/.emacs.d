;;; eqyiel-ox-rkm.el

(require 'org)
(require 'ox-publish)

;; setup custom keywords
(add-to-list 'org-options-keywords "RKM_NEXT:")
(add-to-list 'org-options-keywords "RKM_PREV:")
(add-to-list 'org-options-keywords "RKM_TAGS:")
(add-to-list 'org-export-options-alist '(:next "RKM_NEXT" nil nil t))
(add-to-list 'org-export-options-alist '(:next "RKM_PREV" nil nil t))
(add-to-list 'org-export-options-alist '(:next "RKM_TAGS" nil nil t))


(defvar ox-rkm-sources (org-publish-get-base-files
       (assoc "rkm.id.au-content" org-publish-project-alist)))


(defvar ox-rkm-format-time-string "%Y%m%d%H%M%S")

(setq ox-rkm-sources (org-publish-get-base-files
       (assoc "rkm.id.au-content" org-publish-project-alist)))

(defvar ox-rkm-db ())

(defun ox-rkm-find-property (file key)
  (let* ((org-inhibit-startup t)
          (visitingp (find-buffer-visiting file))
          (work-buffer (or visitingp (find-file-noselect file))))
    (with-current-buffer work-buffer
      (let ((value (plist-get (org-export-get-environment) key)))
        (cond ((equal :date key)
               (format-time-string ox-rkm-format-time-string
                                   (org-publish-find-date file)))
              ((equal :title key)
               (org-no-properties (org-element-interpret-data value)))
              ((equal :next key)
               value)
              (t value))))))

(ox-rkm-find-property "~/doc/rkm.id.au/src/working/ui-shitstorm.org" :next)

(defun ox-rkm-refresh-db ()
  (setq ox-rkm-db '())
  (dolist (filename ox-rkm-sources)
    (let* ((org-inhibit-startup t)
           (visitingp (find-buffer-visiting filename))
           (work-buffer (or visitingp (find-file-noselect filename))))
      (with-current-buffer work-buffer
        (push (ox-rkm-find-property filename :date) ox-rkm-db)
        ;; `add-to-list' adds to the head, so work on `car'
        (setcar ox-rkm-db (plist-put (car ox-rkm-db) :title
                                     (ox-rkm-find-property filename :title)))
        (setcar ox-rkm-db (plist-put (car ox-rkm-db) :src filename)))
      (unless visitingp (kill-buffer work-buffer))))
  (setq ox-rkm-db
        (sort ox-rkm-db
              (lambda (x y)
                (if (string-lessp (cdr (last x)) (cdr (last y))) nil t)))))

(ox-rkm-refresh-db)
;; ((:title "UI Shitstorm" :src "/home/eqyiel/doc/rkm.id.au/src/working/ui-shitstorm.org" . "20150713073300")
;;  (:title "Dynamic dependency management with systemd" :src "/home/eqyiel/doc/rkm.id.au/src/working/dependency-management-with-systemd.org" . "20150712000000")
;;  (:title "Failing early, failing often" :src "/home/eqyiel/doc/rkm.id.au/src/working/01/18/failing-early-failing-often.org" . "20150118000000")
;;  (:title "test.org" :src "/home/eqyiel/doc/rkm.id.au/src/2015/test.org" . "20150713092500")
;;  (:title "Creating beautiful animated GIFs with FFmpeg, ImageMagick and Gifsicle" :src "/home/eqyiel/doc/rkm.id.au/src/2014/creating-beautiful-animated-gifs-with-ffmpeg.org" . "20150118000000"))
;; ox-rkm-db

;; get the second most recent entry
;; (plist-get (nth 1 ox-rkm-db) :src)
;; and the most recent entry
;; (plist-get (car ox-rkm-db) :src)

(defun ox-rkm-make-html (src)
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting src))
         (work-buffer (or visitingp (find-file-noselect src)))
         (html (with-current-buffer work-buffer
                 (org-export-as 'html nil t nil '(:html-doctype "html5" :html-html5-fancy t))
                 )))
    html))

;; (let ((mylst (list (list "freenode" :src 'yep) (list "second" :src 'nope))))
;;   (sort mylst (lambda (x y)
;;                 (if (string-lessp (car x) (car y)) t nil)))
;;   )


;; (defun my-test-handler (httpcon)
;;   "Demonstration function"
;;   ;; (elnode-send-file httpcon (plist-get (car ox-rkm-sources) :src))
;;   (let ((org-html (ox-rkm-make-html ;; "~/doc/rkm.id.au/src/2015/test.org"
;;                    (plist-get (nth 1 ox-rkm-db) :src)
;;                                     )))
;;     (elnode-send-html httpcon org-html)))

;; (elnode-start 'my-test-handler :port 8011 :host "localhost")

;;  (defun elnode-org-handler (httpcon)
;;   (elnode-docroot-for "~/doc/rkm.id.au/src/working/"
;;       with org-file
;;       on httpcon
;;       do (with-current-buffer (find-file-noselect org-file)
;;            (let ((org-html
;;                   ;; This might throw errors so you could condition-case it
;;                   (org-export-as 'html nil t nil)
;;                   ))
;;              (elnode-send-html httpcon org-html)))))

;; (elnode-start 'elnode-org-handler :port 8010 :host "localhost")



;;
