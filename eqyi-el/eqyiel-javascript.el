;;; eqyiel-javascript.el

(setenv "NODE_PATH" "~/.local/lib/node_modules")

;; `tern' cheatsheet:
;;
;; $ npm config set prefix ~/.local
;; $ npm install -g tern
;;
;; Create a file .tern_project in the project directory containing:
;; {
;;     "loadEagerly": [
;;         "js/**.js"
;;     ],
;;     "plugins": {
;;         "node": {
;;         }
;;     }
;; }
;;
;; where "js/**.js" is the path to the .js files.
;; Then start the server.
;; $ tern --host localhost --port 11111 --persistent
;; Now tell emacs where to find it.
;; (tern-use-server 11111 "127.0.0.1")

(autoload 'company-tern "company-tern" nil t)
(autoload 'tern-mode "tern" nil t)
(autoload 'js2-mode "js2-mode" nil t)

(defun eqyiel-company-javascript ()
  (set (make-local-variable 'company-backends) '((company-files
                                                  company-tern
                                                  company-yasnippet)))
  ;; tern is a bit nicer than gtags for javascript
  ;; (set (make-local-variable 'company-backends) '(company-gtags))
  )
;;  >Actually, tern has tern-find-definition which works not only in a single file.
;; I haven't been able to make this work outside my current file. Perhaps I'm just configuring tern wrong :(
;; reply
;; ibabanov 4 hours ago
;; This .tern-project config works for me:
;;   {
;;     "libs": [
;;       "browser"
;;     ],
;;     "plugins": {
;;       "node": {},
;;       "es_modules": {}
;;     }
;;   }
;; Your project files must have "correct" format, e.g. require('relative_path')
;; for imports and module.exports/exports for exports (or es6 import and
;; export). Example for AMD format (requireJS) can be found here -
;; http://ternjs.net/doc/manual.html#configuration

(add-hook 'js2-mode-hook 'eqyiel-company-javascript)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'js2-mode
  '(progn
     (setq js2-highlight-level 3
           js2-idle-timer-delay 3 ;; wait until I'm actually idle
           js2-include-node-externs t
           js2-concat-multiline-strings 'eol
           js2-strict-trailing-comma-warning nil ; cf. airbnb style guide (es6)
           js2-indent-switch-body t
         )
     (define-key js2-mode-map (kbd "C-c C-c") 'projectile-compile-project)
     (define-key js2-mode-map (kbd "C-c j") 'js2-jump-to-definition)))

(eval-after-load 'json-mode
  '(add-hook
    'json-mode-hook
    (lambda ()
      (setq js-indent-level 2
            json-reformat:indent-width 2))))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))

;; (add-hook 'js-mode-hook (lambda () (js2-highlight-unused-variables-mode t)))

(eval-after-load 'js
  '(progn
     (setq js-indent-level 2)
     (define-key
       js-mode-map (kbd "C-c C-c") 'projectile-compile-project)))

(eval-after-load 'tern
  '(define-key tern-mode-keymap (kbd "C-c C-c") 'projectile-compile-project))

(eval-after-load 'flycheck
  (setq flycheck-eslintrc ".eslintrc.json"))

;; (comint-send-string "*nodejs*" "var c = require(`./ColorUtils.js')")

;; Patch 24.5's `sgml-mode' with the new attribute offset feature.
(with-eval-after-load 'sgml-mode

  (defcustom sgml-attribute-offset 0
    "Specifies a delta for attribute indentation in `sgml-indent-line'.

When 0, attribute indentation looks like this:

  <element
    attribute=\"value\">
  </element>

When 2, attribute indentation looks like this:

  <element
      attribute=\"value\">
  </element>"
    :version "25.1"
    :type 'integer
    :safe 'integerp
    :group 'sgml)

  (defun sgml-calculate-indent (&optional lcon)
    "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
    (unless lcon (setq lcon (sgml-lexical-context)))

    ;; Indent comment-start markers inside <!-- just like comment-end markers.
    (if (and (eq (car lcon) 'tag)
             (looking-at "--")
             (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
        (setq lcon (cons 'comment (+ (cdr lcon) 2))))

    (pcase (car lcon)

      (`string
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
                   (zerop (forward-line -1))
                   (looking-at "[ \t]*$")))
       (if (> (point) (cdr lcon))
           ;; Previous line is inside the string.
           (current-indentation)
         (goto-char (cdr lcon))
         (1+ (current-column))))

      (`comment
       (let ((mark (looking-at "--")))
         ;; Go back to previous non-empty line.
         (while (and (> (point) (cdr lcon))
                     (zerop (forward-line -1))
                     (or (looking-at "[ \t]*$")
                         (if mark (not (looking-at "[ \t]*--"))))))
         (if (> (point) (cdr lcon))
             ;; Previous line is inside the comment.
             (skip-chars-forward " \t")
           (goto-char (cdr lcon))
           ;; Skip `<!' to get to the `--' with which we want to align.
           (search-forward "--")
           (goto-char (match-beginning 0)))
         (when (and (not mark) (looking-at "--"))
           (forward-char 2) (skip-chars-forward " \t"))
         (current-column)))

      ;; We don't know how to indent it.  Let's be honest about it.
      (`cdata nil)
      ;; We don't know how to indent it.  Let's be honest about it.
      (`pi nil)

      (`tag
       (goto-char (+ (cdr lcon) sgml-attribute-offset))
       (skip-chars-forward "^ \t\n")    ;Skip tag name.
       (skip-chars-forward " \t")
       (if (not (eolp))
           (current-column)
         ;; This is the first attribute: indent.
         (goto-char (+ (cdr lcon) sgml-attribute-offset))
         (+ (current-column) sgml-basic-offset)))

      (`text
       (while (looking-at "</")
         (forward-sexp 1)
         (skip-chars-forward " \t"))
       (let* ((here (point))
              (unclosed (and ;; (not sgml-xml-mode)
                         (looking-at sgml-tag-name-re)
                         (assoc-string (match-string 1)
                                       sgml-unclosed-tags 'ignore-case)
                         (match-string 1)))
              (context
               ;; If possible, align on the previous non-empty text line.
               ;; Otherwise, do a more serious parsing to find the
               ;; tag(s) relative to which we should be indenting.
               (if (and (not unclosed) (skip-chars-backward " \t")
                        (< (skip-chars-backward " \t\n") 0)
                        (back-to-indentation)
                        (> (point) (cdr lcon)))
                   nil
                 (goto-char here)
                 (nreverse (sgml-get-context (if unclosed nil 'empty)))))
              (there (point)))
         ;; Ignore previous unclosed start-tag in context.
         (while (and context unclosed
                     (eq t (compare-strings
                            (sgml-tag-name (car context)) nil nil
                            unclosed nil nil t)))
           (setq context (cdr context)))
         ;; Indent to reflect nesting.
         (cond
          ;; If we were not in a text context after all, let's try again.
          ((and context (> (sgml-tag-end (car context)) here))
           (goto-char here)
           (sgml-calculate-indent
            (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
                      (sgml-tag-type (car context)) 'tag)
                  (sgml-tag-start (car context)))))
          ;; Align on the first element after the nearest open-tag, if any.
          ((and context
                (goto-char (sgml-tag-end (car context)))
                (skip-chars-forward " \t\n")
                (< (point) here) (sgml-at-indentation-p))
           (current-column))
          (t
           (goto-char there)
           (+ (current-column)
              (* sgml-basic-offset (length context)))))))

      (_
       (error "Unrecognized context %s" (car lcon)))

      )))
(require 'sgml-mode)

(provide 'eqyiel-javascript)
