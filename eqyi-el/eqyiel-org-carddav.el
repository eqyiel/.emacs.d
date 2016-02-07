;; http://sabre.io/dav/building-a-carddav-client/

;; vCard 3.0 RFC: https://tools.ietf.org/html/rfc6350
;; vCard 4.0 RFC: https://tools.ietf.org/html/rfc6350

(require 'org-vcard)
(require 'url-dav)

(defvar org-carddav-url "https://cloud.rkm.id.au/remote.php/carddav/addressbooks/"
  "Base URL for CardDAV access.")

(defvar org-carddav-user-id "eqyiel"
  "ID of your user.")

(defvar org-carddav-uuid-extension ".vcf"
  "The file extension to add to uuids in webdav requests.
This is usually .vcf.")

(defvar org-carddav-empty-addressbook nil
  "Flag if we have an empty addressbook in the beginning.")

(defvar org-carddav-debug-level 1
  "Level of debug output in `org-carddav-debug-buffer'.
0 or nil: no debug output.  1: Normal debugging.  2: Excessive debugging (this
will also output contact into the buffer).")

(defvar org-carddav-debug-buffer "*org-carddav-debug*"
  "Name of the debug buffer.")

(defun org-carddav-addressbook-url ()
  "Return URL for addressbook."
  (if (string-match "google\\.com" org-carddav-url)
      (concat (directory-file-name org-carddav-url) "/" org-carddav-user-id "/events/")
    (concat (directory-file-name org-carddav-url) "/" org-carddav-user-id "/contacts/")))

(defun org-carddav-debug-print (level &rest objects)
  "Print OBJECTS into debug buffer with debug level LEVEL.
Do nothing if LEVEL is larger than `org-carddav-debug-level'."
  (unless (or (null org-carddav-debug-level)
        (> level org-carddav-debug-level))
    (with-current-buffer (get-buffer-create org-carddav-debug-buffer)
      (dolist (cur objects)
  (if (stringp cur)
      (insert cur)
    (prin1 cur (current-buffer)))
  (insert "\n")))))

(defun org-carddav-get-vcffiles-etags-from-properties (properties)
  "Return all vcf files and etags from PROPERTIES."
  (let (prop files)
    (while (setq prop (pop properties))
      (let ((url (car prop))
            (etag (plist-get (cdr prop) 'DAV:getetag)))
        (if (string-match (concat ".*/\\(.+\\)\\" org-carddav-uuid-extension "/?$") url)
            (setq url (match-string 1 url))
          (setq url nil))
        (when (string-match "\"\\(.*\\)\"" etag)
          (setq etag (match-string 1 etag)))
        (when (and url etag)
          (push (cons (url-unhex-string url) etag) files))))
    files))

(defun org-carddav-get-event-etag-list ()
  "Return list of events with associated etag from remote.
Return list with elements (uid . etag)."
  (if org-carddav-empty-addressbook
      nil
    (let ((output (url-dav-get-properties
                   (org-carddav-addressbook-url)
                   '(DAV:getetag) 1)))
      (cond
       ((> (length output) 1)
        ;; Everything looks OK - we got a list of "things".
        ;; Get all vcf files and etags you can find in there.
        (org-carddav-get-vcffiles-etags-from-properties output))
       ((or (null output)
            (zerop (length output)))
        ;; This is definitely an error.
        (error "Error while getting addressbook from %s." (org-carddav-addressbook-url)))
       ((and (= (length output) 1)
             (stringp (car-safe (car output))))
        (let ((status (plist-get (cdar output) 'DAV:status)))
          (if (eq status 200)
              ;; This is an empty directory
              'empty
            (if status
                (error "Error while getting addressbook from %s. Got status code: %d."
                       (org-carddav-events-url) status)
              (error "Error while getting addressbook from %s."
                     (org-carddav-addressbook-url))))))))))

(defun org-carddav-get-event (uid &optional with-headers)
  "Get contact with UID from addressbook.
Function returns a buffer containing the contact, or nil if there's
no such contact.
If WITH-HEADERS is non-nil, do not delete headers."
  (org-carddav-debug-print 1 (format "Getting event UID %s." uid))
  (with-current-buffer
      (url-retrieve-synchronously
       (concat (org-carddav-addressbook-url) (url-hexify-string uid) org-carddav-uuid-extension))
    (goto-char (point-min))
    (when (search-forward "BEGIN:VCARD" nil t)
      (beginning-of-line)
      (unless with-headers
        (delete-region (point-min) (point)))
      (save-excursion
        (while (re-search-forward "\^M" nil t)
          (replace-match "")))
      ;; Join lines because of bug in icalendar parsing.
      ;; (save-excursion
      ;; (while (re-search-forward "^ " nil t)
      ;;   (Delete-char -2)))
      (org-carddav-debug-print 2 (format "Content of event UID %s: " uid)
                              (buffer-string))
      (current-buffer))))

;; (org-carddav-get-vcffiles-etags-from-properties
;; '(("/remote.php/carddav/addressbooks/eqyiel/contacts/" DAV:getetag "" DAV:status 404)))
;; (let* ((url-request-method "REPORT")
;;        (url-request-data        ;;  get every single contact
;;         "<card:addressbook-query xmlns:d=\"DAV:\" xmlns:card=\"urn:ietf:params:xml:ns:carddav\">
;;     <d:prop>
;;         <d:getetag />
;;         <card:address-data />
;;     </d:prop>
;; </card:addressbook-query>")
;;       ;; (url-request-data ;; get just one contact with FN containing "Ruben"
;;    ;; "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
;;    ;; <C:addressbook-query xmlns:D=\"DAV:\"
;;    ;;                   xmlns:C=\"urn:ietf:params:xml:ns:carddav\">
;;    ;;   <D:prop>
;;    ;;     <D:getetag/>
;;    ;;     <C:address-data>
;;    ;;       <C:prop name=\"VERSION\"/>
;;    ;;       <C:prop name=\"UID\"/>
;;    ;;       <C:prop name=\"NICKNAME\"/>
;;    ;;       <C:prop name=\"EMAIL\"/>
;;    ;;       <C:prop name=\"FN\"/>
;;    ;;     </C:address-data>
;;    ;;   </D:prop>
;;    ;;   <C:filter>
;;    ;;     <C:prop-filter name=\"FN\">
;;    ;;       <C:text-match collation=\"i;unicode-casemap\"
;;    ;;                     match-type=\"contains\"
;;    ;;       >Ruben</C:text-match>
;;    ;;     </C:prop-filter>
;;    ;;   </C:filter>
;;    ;; </C:addressbook-query>")
;;       (url-request-extra-headers
;;        `(("Depth" . "1")
;;          ("Content-Type" . "text/xml; charset=\"utf-8\"")
;;          ("Authorization" .
;;           ,(concat "Basic "
;;                    (base64-encode-string
;;                     (concat "eqyiel" ":" "9LO3tkaWMT@O485F"))))
;;          ;; ("Content-Length" . ,(number-to-string (length url-request-data)))
;;          )))
;;   (url-retrieve "https://cloud.rkm.id.au/remote.php/carddav/addressbooks/eqyiel/contacts/"
;;                 (lambda (status)
;;                   (set-buffer-multibyte t)
;;                   (switch-to-buffer (current-buffer))
;;                   (let ((start (save-excursion
;;                                  (goto-char (point-min))
;;                                  (and (re-search-forward "<\?xml" (point-max) t)
;;                                       (match-beginning 0)))))
;;                     ;;(identica-clean-response-body) ; necessary for identica, cleaned up some weird characters
;;                     (and start
;;                          (message "%s" (nth 2 (car (xml-parse-region start (point-max))))))))))



;; (let* ((url-request-method "PROPFIND")
;;        (url-request-data
;;         "<d:propfind xmlns:d=\"DAV:\" xmlns:cs=\"http://calendarserver.org/ns/\">
;;   <d:prop>
;;      <d:displayname />
;;      <cs:getctag />
;;      <d:sync-token />
;;   </d:prop>
;; </d:propfind>")
;;       (url-request-extra-headers
;;        `(("Depth" . "0")
;;          ("Content-Type" . "text/xml; charset=\"utf-8\"")
;;          ("Authorization" .
;;           ,(concat "Basic "
;;                    (base64-encode-string
;;                     (concat "eqyiel" ":" "9LO3tkaWMT@O485F"))))
;;          ;; ("Content-Length" . ,(number-to-string (length url-request-data)))
;;          )))
;;   (url-retrieve "https://cloud.rkm.id.au/remote.php/carddav/addressbooks/eqyiel/contacts/"
;;                 (lambda (status) (switch-to-buffer (current-buffer)))))


;; ;; HTTP/1.1 207 Multi-Status
;; ;; Server: nginx/1.9.6
;; ;; Date: Tue, 05 Jan 2016 01:35:01 GMT
;; ;; Content-Type: application/xml; charset=utf-8
;; ;; Transfer-Encoding: chunked
;; ;; Connection: keep-alive
;; ;; Expires: Thu, 19 Nov 1981 08:52:00 GMT
;; ;; Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0
;; ;; Pragma: no-cache
;; ;; Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; frame-src *; img-src * data: blob:; font-src 'self' data:; media-src *; connect-src *
;; ;; X-XSS-Protection: 1; mode=block
;; ;; X-Content-Type-Options: nosniff
;; ;; X-Frame-Options: Sameorigin
;; ;; X-Robots-Tag: none
;; ;; X-Sabre-Version: 2.1.7
;; ;; Vary: Brief,Prefer
;; ;; DAV: 1, 3, extended-mkcol, addressbook, access-control, calendarserver-principal-property-search

;; ;; <?xml version="1.0" encoding="utf-8"?>
;; ;; <d:multistatus xmlns:d="DAV:" xmlns:s="http://sabredav.org/ns" xmlns:card="urn:ietf:params:xml:ns:carddav">
;; ;;   <d:response>
;; ;;     <d:href>/remote.php/carddav/addressbooks/eqyiel/contacts/</d:href>
;; ;;     <d:propstat>
;; ;;       <d:prop>
;; ;;         <d:displayname>Contacts</d:displayname>
;; ;;         <x3:getctag xmlns:x3="http://calendarserver.org/ns/">1451889349</x3:getctag>
;; ;;       </d:prop>
;; ;;       <d:status>HTTP/1.1 200 OK</d:status>
;; ;;     </d:propstat>
;; ;;     <d:propstat>
;; ;;       <d:prop>
;; ;;         <d:sync-token/>
;; ;;       </d:prop>
;; ;;       <d:status>HTTP/1.1 404 Not Found</d:status>
;; ;;     </d:propstat>
;; ;;   </d:response>
;; ;; </d:multistatus>

;; (url-dav-get-properties "https://cloud.rkm.id.au/remote.php/carddav/addressbooks/eqyiel/contacts/" '(DAV:getetag) 1)

;; (org-caldav-get-icsfiles-etags-from-properties
;;  '(("/remote.php/carddav/addressbooks/eqyiel/contacts/0ff502ff-f506-47b6-aa76-80ee5ec27dc9.vcf" DAV:getetag "\"f5b17f6671bf37c88e33ecc19f0377fb\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/3c90b13a-f261-4bbf-9f64-eebac05f89bf.vcf" DAV:getetag "\"68e5a253058ee9bfb41e5039a0b5d9de\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/27f53248-da85-46ff-8dbb-238c59019f2a.vcf" DAV:getetag "\"832ebc3d7f2a31feb06e2ff81336c132\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/5e5fb106-0a35-47e5-83c6-0dc9648cfae0.vcf" DAV:getetag "\"46d78560a1c0e4edba66c4eb16a301f3\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/bfa3bcab-775b-4635-8bda-688212c90fab.vcf" DAV:getetag "\"fa6502d5425dbc98f101eca5d3700b28\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/1f7d9255-1531-4c3d-965d-bcb14f287f9c.vcf" DAV:getetag "\"8538721f5efc740560b9d5115fc42521\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/354d6ba7-613f-4ac8-98cb-d5af992aabf6.vcf" DAV:getetag "\"b3648dd8fd3e646a6b5eacad000ea35a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/9133100c-7981-4e6c-a8d3-247f2c491c9a.vcf" DAV:getetag "\"50895326beebc4349b37ab92862c95b8\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/0c1a88a0-753e-4e47-952c-8be0fd5dd474.vcf" DAV:getetag "\"6572ba9bbfefb7265acd993a9911994d\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/ea49d821-2250-425d-bfb4-88f05193321b.vcf" DAV:getetag "\"caf978562792b825b2f8f53646c2c1e6\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/4f866ae4-07e5-4449-be72-4c9d6627faa4.vcf" DAV:getetag "\"525cd3672615ee13e33bf97835dcfe15\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/ac1ce9fd-fba7-4be6-913f-e108aac79de1.vcf" DAV:getetag "\"fbfb955001317de60ebb854391cc44b3\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/1a108fea-e6c0-41dd-afd6-9d9990e98684.vcf" DAV:getetag "\"c50c70b9408f6a1297dad6b30b692fae\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/8a67c4ce-0b48-4dcd-8896-1a4887e4ea08.vcf" DAV:getetag "\"4026346b310db78b756bd2073644823b\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/e469f136-f9af-490a-b2f0-90890c72dbcd.vcf" DAV:getetag "\"0ff828803e844655e83145d2687ecfa5\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/a6590969-9156-44ce-b5b7-87753fe2b990.vcf" DAV:getetag "\"7c7645bb9275528c5cfacc983e402f03\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/d9445b1e-82f8-4daf-93c9-4e0e3e4bb07a@cloud.rkm.id.au.vcf" DAV:getetag "\"1f0bd6615db8f390dd2df629a08cc1e0\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/e0afc694-010e-45a2-a679-522b4a2076b3.vcf" DAV:getetag "\"adb7475874ecf54f95840a2eb18a8986\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/68ec2b57-5253-4cd3-bc21-e2bb95f372a3.vcf" DAV:getetag "\"88d9fa9d0e84fe8f0ac3ea70ff06246f\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/cc2054ba-601f-4537-b89d-67d7a3b1e3a3@cloud.rkm.id.au.vcf" DAV:getetag "\"f3e083da4f476664ed6d5026d110fa5c\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/c5b2a990-2b32-41ae-81c7-6c6413e0c46f.vcf" DAV:getetag "\"fce92d57f6bf90c6af1cdbaaf1ac6c55\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/c2415fc8-f992-470c-b6cf-2e5ce9971bb7.vcf" DAV:getetag "\"a981756f4fbea97f99be761519ea17a0\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/6080a597-9cb6-4cd4-bf4a-af7e0340cad7.vcf" DAV:getetag "\"4d594e363b493ebc7a1175f8d5cb5693\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/7b158e56-0882-4742-abcc-cfc2daf3b777.vcf" DAV:getetag "\"7c6e17bb9684cccfe31a333b78b21206\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/ab3f514c-f9dc-460e-b7eb-eb6fd9c3a924.vcf" DAV:getetag "\"ca8b3bff188d74c79c7e1be25f50972a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/5e297ce1-3486-4f06-a521-72242a9521f2.vcf" DAV:getetag "\"b448d7af45698e87234851c016325acf\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/54f19a1a-6e85-4abd-aeac-27924ca10bc2.vcf" DAV:getetag "\"9ebe0303a271cc28d363f473fd479292\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/44ac868c-d889-4d65-8bb6-1460607d1566.vcf" DAV:getetag "\"0317524d27afe4846ead29d631658912\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/136c7596-738c-49f3-8b00-e468ab34b819.vcf" DAV:getetag "\"7368b7237c35d988edf869d03eb35c26\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/de723ded-b12c-4f05-a52c-ca74f6221f31.vcf" DAV:getetag "\"a4681fbad5d77c9e64b8440498de8e1a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/96248514-e18d-49ee-bb49-3ecae8789254.vcf" DAV:getetag "\"59665dd1c7adad185acae746a98ada48\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/c0a37e26-290d-4814-b2fb-ea8baa5757f1.vcf" DAV:getetag "\"5a4d178fbba75d3d245a4c0bff4701b7\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/155bf607-28f5-4a89-99d4-3e2f25c26b0b.vcf" DAV:getetag "\"7bbf3212f8552cf8674d201f6e6399cf\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/68f1a8ad-97b1-4dbc-9881-a131cbc925f0.vcf" DAV:getetag "\"7a99557497e7503fd078aa80afcfea48\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/09453833-9fab-45de-8ce6-17ee19124309.vcf" DAV:getetag "\"1cdac3b2225eadcdd16c053eacc4793d\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/945f1329-3bb2-4d88-ae13-b3bdbccac518@cloud.rkm.id.au.vcf" DAV:getetag "\"1305b091914bca285711fde5ab14c8e2\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/60a28f4f-d417-404b-add8-849dc6b14523.vcf" DAV:getetag "\"9c3c91814219d589d9d911e7666b5039\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/3b8f7581-b887-49a5-80a2-85c56d7379d2.vcf" DAV:getetag "\"315c6723f6bec6a7fe5cc360943fbac4\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/b3b7e4e7-8da0-46e2-9e9d-5a0029d2a4e9.vcf" DAV:getetag "\"94c3bf3a79c148cc82dfaae511441832\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/2705ef4d-8263-4e45-a765-73e8b70abb04.vcf" DAV:getetag "\"9bbd8624e26257cdc078be4b877b637e\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/264e5bb1-3beb-49b9-ad4e-d6b910399e13.vcf" DAV:getetag "\"2a049d2b37e022bbba24301ef39f0744\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/86f9f3d4-f11b-40b7-ac94-5a33f3df55ac@cloud.rkm.id.au.vcf" DAV:getetag "\"555d63be02b15a2eb759704cea8a81cd\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/6a266d19-be48-4188-9ad2-be924bdb4668@cloud.rkm.id.au.vcf" DAV:getetag "\"851b0ee0cf423914ba2496e5b5325a84\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/66b494e6-eb45-4cd4-8127-95653a0a12d1@cloud.rkm.id.au.vcf" DAV:getetag "\"b12f5d71c1b4b67c4befa54acb689869\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/c017eff0-68c6-4c77-adbd-cd50238da551@cloud.rkm.id.au.vcf" DAV:getetag "\"1aed037c1c4741e60489c09265576477\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/194d9c42-8782-4355-b70e-0301ffde1aa0@cloud.rkm.id.au.vcf" DAV:getetag "\"cf020e140fcfbd8278cda4779936126c\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/a0b921b4-9ba7-44ce-90a4-de4cfe6df80e@cloud.rkm.id.au.vcf" DAV:getetag "\"624e77af7b0c4dcb20b81ebb58158633\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/b9265f5a-e7e3-44db-ac7e-a13244114b0b@cloud.rkm.id.au.vcf" DAV:getetag "\"48d4f62d4d1182d92f3ee453a7e4d8dd\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/e0cee8b8-7f37-44cc-96ff-9f95619a4b88@cloud.rkm.id.au.vcf" DAV:getetag "\"95ba4464103eb41ed53875ccc1a6d8c9\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/d3d8dfea-737c-44be-bcb2-2caa4860e199@cloud.rkm.id.au.vcf" DAV:getetag "\"1334aec1b87b2af982f40581bc7a6956\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/73b001db-581f-4670-a236-244812316755@cloud.rkm.id.au.vcf" DAV:getetag "\"d9697cf3e91589bdbff2e014e69e550e\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/d20c79c5-c6fa-4ba3-ab40-db6f9e2becb3@cloud.rkm.id.au.vcf" DAV:getetag "\"e712003aafc26b6e21fb9806c6ad63ea\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/318cd059-c21c-47e7-95cc-a1df7822c7e2@cloud.rkm.id.au.vcf" DAV:getetag "\"1461b4bc88ee68f2412bcbda0890024d\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/cce89d01-7cb2-4346-bdf7-49c21e7ad8af@cloud.rkm.id.au.vcf" DAV:getetag "\"bce79f4664ed3f7dcc90f5a6b0402e99\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/58792488-b3f9-401a-b780-a335ef54d363@cloud.rkm.id.au.vcf" DAV:getetag "\"d18db45e8308963e915f67a8e7b6e7b8\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/14376fde-8ccf-4d00-9f17-d1cbc0d5baa4@cloud.rkm.id.au.vcf" DAV:getetag "\"d190ed2dc24002c739ffda2da696748f\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/cfb32821-bbf1-47c4-9f5d-c25e7fd64138@cloud.rkm.id.au.vcf" DAV:getetag "\"f2d9fe65d852c9ffc7e7a42e7c5e2bb5\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/1a81c5e7-ddbd-4155-882a-cecf0f53f3a7@cloud.rkm.id.au.vcf" DAV:getetag "\"222784cd92fc81df4b67df5de587559c\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/209cf9fd-30a6-4476-bbfd-86cddc2c5fd3@cloud.rkm.id.au.vcf" DAV:getetag "\"0a6c685e8b9a820b24b01f1b5a77f20d\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/28b7a3d2-7177-409b-a622-9a3e8ea46e5e@cloud.rkm.id.au.vcf" DAV:getetag "\"c502ec9dde94a89b9c1973b21abed699\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/11196c8a-01a5-4495-bc3c-89b517675845@cloud.rkm.id.au.vcf" DAV:getetag "\"189a5a5cb91dc80e327815a557884b04\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/37cadd11-fbbb-496b-9087-0dfe1db4deda@cloud.rkm.id.au.vcf" DAV:getetag "\"630ca705671fe822d43137eb450c0e01\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/9cb2b45b-d17d-42f5-a050-c750793d9d54@cloud.rkm.id.au.vcf" DAV:getetag "\"693f252da271fce7ea7b3d153a221a18\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/b34be63b-3637-4386-87c2-98476424ae6b@cloud.rkm.id.au.vcf" DAV:getetag "\"c283cf0a70e898bbe46d3ac598789323\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/09312176-03c2-4854-a380-29ca5bd624cb@cloud.rkm.id.au.vcf" DAV:getetag "\"149a507ad72d9072fc605205ffcfe265\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/820bf0f0-cbe4-4a33-b79b-ec02a7d3e316@cloud.rkm.id.au.vcf" DAV:getetag "\"1aabd722413c0651ed73957de6d2b7e6\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/660edef0-0347-4699-a193-9b85a120ee42@cloud.rkm.id.au.vcf" DAV:getetag "\"b00a1a3244ea30f03fa5ee4a1bf3afbb\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/064261d6-9d4b-4301-889f-5c282a7f0e2e@cloud.rkm.id.au.vcf" DAV:getetag "\"6b0cd0488c5567a142fb3ce727bb86b8\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/60f701fc-9f9c-45c9-844d-b8c617401de0@cloud.rkm.id.au.vcf" DAV:getetag "\"cd356131d92fa3a4df22bc389dd14bb5\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/de29d9b7-1204-4fc8-9379-991880b654a2@cloud.rkm.id.au.vcf" DAV:getetag "\"8265ec430a04ab9b574da0de1c22df34\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/9acc6043-8d36-4e04-93f1-84f2aac26026@cloud.rkm.id.au.vcf" DAV:getetag "\"ed6520eccf51568c6bb5f6689d26b9c5\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/844a3704-7cdc-4bf1-b0ea-9c3903486fa3@cloud.rkm.id.au.vcf" DAV:getetag "\"ef541bbfb2dbf256e3df2918c34aa5a7\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/bd428f30-ab6a-4e38-aa5e-f7be18f54af4@cloud.rkm.id.au.vcf" DAV:getetag "\"23c0337ad26e88cbd404a85d9e570be4\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/d23cf5e2-fcb5-4325-90c5-4fb611886784@cloud.rkm.id.au.vcf" DAV:getetag "\"ae95770e61b2f178ade56457e3fa5a53\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/16d30b6b-296a-4789-9860-2ae1b3c516bb@cloud.rkm.id.au.vcf" DAV:getetag "\"f88d95315422bc0da80340f69e09c00a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/807d7fff-f880-41b8-a2af-37e4deb81727@cloud.rkm.id.au.vcf" DAV:getetag "\"5c0c1028adbf7fe0eacc41fae9781c35\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/f2bd7082-d8ba-42de-816c-fa914c55cd3e@cloud.rkm.id.au.vcf" DAV:getetag "\"560bef5173f932dedefe28a7ead0c360\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/588afa64-cf09-4da9-a9de-c5ff2097c199@cloud.rkm.id.au.vcf" DAV:getetag "\"451e22fb923988b34457ebde0d9e91e9\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/52033597-eded-447a-91f2-0b4212340089@cloud.rkm.id.au.vcf" DAV:getetag "\"16e2054e1d81c5b3720a2c41e0877a60\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/5ba4f81a-884c-4ee3-aa52-8ea8c35d7c10@cloud.rkm.id.au.vcf" DAV:getetag "\"5d1309a9bef358b4d9f5d6966245131a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/94256c3f-3e9c-4bf5-bef3-6a3851882f92@cloud.rkm.id.au.vcf" DAV:getetag "\"736fd8e07d8308b6cb1e2f9e5b8cbecc\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/c408ba08-43de-47cb-abe7-33f9ace619d0@cloud.rkm.id.au.vcf" DAV:getetag "\"87803cd9d861982825dc3d2ccf04e5e7\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/96e6811f-d344-4af7-9e90-42d92937c81b@cloud.rkm.id.au.vcf" DAV:getetag "\"49f99157a45bda7285295500fe25b4a9\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/bc0d7c02-11aa-41e9-84c4-3a32d9669442@cloud.rkm.id.au.vcf" DAV:getetag "\"aa83462709a2b5d8464986f3322382f6\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/3a0e3ba7-979d-405d-aa74-210708acea48@cloud.rkm.id.au.vcf" DAV:getetag "\"728006d1d31646ed5d88674f80ca49db\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/f4aaad76-83bb-45b2-84b5-dd40eef2ef00@cloud.rkm.id.au.vcf" DAV:getetag "\"95bab40995a542e6c17f61631ab2172f\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/fc4f4a06-72b9-4bbb-b397-969a4f8a9b34@cloud.rkm.id.au.vcf" DAV:getetag "\"5c222e93f787b362ea55a2f83d9cd599\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/09c7d3c8-34ec-4cab-a9fa-69f9eaae2668@cloud.rkm.id.au.vcf" DAV:getetag "\"26dccf5914f1e9ff95bbd882503d9844\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/da11c140-640f-4fea-86da-b80fa465631c@cloud.rkm.id.au.vcf" DAV:getetag "\"88168656c2d8eb1a692541d9742bbfab\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/e775375d-f1bc-4721-ac8c-e7a2b54b2943@cloud.rkm.id.au.vcf" DAV:getetag "\"c541b5b1a7601f0f79a747abc08a960c\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/ae5c154b-3325-41b9-af3e-200a82d48a9c@cloud.rkm.id.au.vcf" DAV:getetag "\"6ed0e5df44067a4561e23bd3e07cf244\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/7adab358-e318-4fa7-be39-1ce2ace53cc1@cloud.rkm.id.au.vcf" DAV:getetag "\"c646fca0e85b91e770b57e994394934a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/12f185d0-2bf1-4dc3-93d5-e29c0f5f7241@cloud.rkm.id.au.vcf" DAV:getetag "\"26f9e2ba09eeeb734e90aa7d78e2fb01\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/3fecca82-ab7c-419f-b982-47f8890b7529@cloud.rkm.id.au.vcf" DAV:getetag "\"ba6eb9c71c565a20ea72424aab2891df\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/6a9c374e-28e8-4d70-8000-d57cb0bd1573@cloud.rkm.id.au.vcf" DAV:getetag "\"d4db04cfab8fb88fcbba34dc0d83574b\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/88e5ca33-ff03-49bd-ab6b-4d3c914055f2@cloud.rkm.id.au.vcf" DAV:getetag "\"8f96cb4449cfab3646301840474647ea\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/9ac78cd1-8172-4b4b-bd54-adb8190e6c75@cloud.rkm.id.au.vcf" DAV:getetag "\"d94dbc078b82a2074b54af4e628b5790\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/1e7d4ace-9c8e-418e-adea-deff01d3c375@cloud.rkm.id.au.vcf" DAV:getetag "\"93da3b7469d32960961459fd42529145\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/bf6d0bb0-e5da-4247-abcd-76a09e2c086d@cloud.rkm.id.au.vcf" DAV:getetag "\"f8930dcc26a89590b2c0c8861157feef\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/7f4e5ebb-9fec-487e-afe5-92ffab126494@cloud.rkm.id.au.vcf" DAV:getetag "\"e5226438b6554a0f3d0697bbb896a26e\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/8a3c15a8-ded8-49fd-bde4-aa1b18c30094@cloud.rkm.id.au.vcf" DAV:getetag "\"d42728a1ff8562887cc0e158dc9b3c8e\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/7defd0e4-b1a7-458d-8d7d-23bc0fa9055f@cloud.rkm.id.au.vcf" DAV:getetag "\"73c659fcef05309eeec74ee029b1304a\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/442d0a3f-5337-4a1f-a6dc-295fbdf73eec@cloud.rkm.id.au.vcf" DAV:getetag "\"e2d18427f6018734a290456c1dbb2306\"" DAV:status 200)
;;   ("/remote.php/carddav/addressbooks/eqyiel/contacts/" DAV:getetag "" DAV:status 404)))


(defun org-carddav--contact-property (property contact)
  "Return value of first property named PROPERTY in CONTACT.
Return nil if PROPERTY is not in CONTACT."
  (assoc-default property contact #'string= nil))



(setq org-carddav-debug-level 2)
(setq org-carddav-test-files (org-carddav-get-event-etag-list))
(with-current-buffer (org-carddav-get-event (caar org-carddav-test-files))
  (org-vcard-import-parse "buffer")
  ;; (org-vcard-import 4)
  )

;; (org-carddav--contact-property "BDAY;VALUE=DATE" '(("VERSION" . "3.0") ("UID" . "442d0a3f-5337-4a1f-a6dc-295fbdf73eec@cloud.rkm.id.au") ("FN" . "Alison Sloley") ("N" . "Sloley;Alison;;;") ("TEL;TYPE=\"cell\"" . "0435266066") ("BDAY;VALUE=DATE" . "1965-12-07") ("PRODID" . "DAVdroid/0.6.2 (ez-vcard/0.9.5)") ("REV" . "2015-12-07T10:25:07+00:00")))


;; (defvar org-carddav-vcard-properties-spec
;;   ;; NAME VALUE-TYPE CARDINALITY
;;   '(("BEGIN" 'text 1)
;;     ("END" 'text 1)
;;     ("SOURCE" 'uri *)
;;     ("KIND" 'text *1) ; individual/group/org/location/x-name/iana-token
;;     ("XML" 'text *)
;;     ("FN" 'text 1*)
;;     ("N" 'text *1) ;family names, given names, additional names, prefixes,
;;                    ;honorific suffixes
;;     ("NICKNAME" 'text *)
;;     ("PHOTO" 'uri *)
;;     ))

(setq org-carddav-vcard-properties-spec
      '(("FN" . org-carddav--FN-handler)
        ("N" . org-carddav--N-handler)
        ("BDAY" . org-carddav--BDAY-handler)))

(defun org-carddav--FN-handler (fn)
  (cons "FN" fn))

(defun org-carddav--N-handler (n)
  (cons "N" n))

(defun org-carddav--BDAY-handler (bday)
  (cons "BDAY" bday))

;; (directory-files "~/contacts/contacts" t "\.vcf$" t)

;; (org-carddav-parse-vcf (nth 80 (directory-files "~/contacts/contacts" t "\.vcf$" t)))

;; (org-carddav-parse-vcf "~/contacts/contacts/2705ef4d-8263-4e45-a765-73e8b70abb04.vcf")

(defun org-carddav-parse-vcf (filename)
  (let ((current-line nil)
        (property "")
        (value "")
        (cards '())
        (current-card '()))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (setq case-fold-search t)
      (while (re-search-forward "BEGIN:VCARD" (point-max) t)
        (setq current-card '())
        (forward-line)
        (while (not (looking-at "END:VCARD"))
          (setq current-line
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (string-match "\\([^:]+\\): *\\(.*?\\)\\(?:\u000D\\|\015\\)?$" current-line)
          (setq property (match-string 1 current-line))
          (setq value (match-string 2 current-line))
          (forward-line)
          (setq current-line
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (when (assoc property org-carddav-vcard-properties-spec)
            (funcall (cdr (assoc property org-carddav-vcard-properties-spec)) value))
          ;; (mapcar (lambda (arg) (if (string-equal (car arg) property)
          ;;                           ;; process value based on property
          ;;                           (funcall (cadr arg) value)))
          ;;         org-carddav-vcard-properties-spec)
          ;; (while (not (string-match "^\\([^\u0009\u0020\011\040:]+\\):" current-line))
          ;;   (string-match "^\\(?:\u0009\\|\u0020\\|\011\\|\040\\)\\(.*?\\)\\(?:\u000D\\|\015\\)?$" current-line)
          ;;   (setq value (concat value (match-string 1 current-line)))
          ;;   (forward-line)
          ;;   (setq current-line
          ;;         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          ;; (if (string-match ";CHARSET=\\([^;:]+\\)" property)
          ;;     (let ((encoding (match-string 1 property)))
          ;;       (setq property (replace-regexp-in-string ";CHARSET=[^;:]+" "" property))
          ;;       (cond
          ;;        ((or (string= "4.0" org-vcard-active-version)
          ;;             (string= "3.0" org-vcard-active-version))
          ;;         ;; vCard 4.0 mandates UTF-8 as the only possible encoding,
          ;;         ;; and 3.0 mandates encoding not per-property, but via the
          ;;         ;; CHARSET parameter on the containing MIME object. So we
          ;;         ;; just ignore the presence and/or value of the CHARSET
          ;;         ;; modifier in 4.0 and 3.0 contexts.
          ;;         t)
          ;;        ((string= "2.1" org-vcard-active-version)
          ;;         (setq value (string-as-multibyte
          ;;                      (encode-coding-string
          ;;                       value
          ;;                       (cdr (assoc encoding org-vcard-character-set-mapping)))))))))
          ;; (setq property (org-vcard-canonicalise-property-name property))
          ;; (setq current-card (append current-card (list (cons property value))))
          )
        (setq cards (append cards (list current-card)))))
    current-card))
