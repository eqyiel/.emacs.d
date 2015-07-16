;; eqyiel-lib.el
;; Most of these are taken from other people.  Where possible, a link to the
;; original author precedes the function.

;; http://david.rothlis.net/emacs/ergonomics.html
(defun eqyiel-kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'eqyiel-kill-region-or-backward-kill-word)

;; https://github.com/technomancy/emacs-starter-kit/
(defun eqyiel-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'eqyiel-local-comment-auto-fill)

;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun eqyiel-sudo-edit (&optional arg)
  "Edit currently visited file as root.  With a prefix ARG prompt for a file to
visit.  Will also prompt for a file to visit if current buffer is not visiting a
file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eqyiel-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eqyiel-insert-date ()
  "Insert today's date."
  (interactive)
  (let ((t0 (current-time)))
    (insert (completing-read "Select format: "
                     `(,(format-time-string "<%F %a %T>" t0)
                       ,(format-time-string "<%F %a>" t0)
                       ,(format-time-string "%s" t0)
                       ,(format-time-string "%R" t0)
                       ,(format-time-string "%T" t0))))))
(global-set-key (kbd "C-c d") 'eqyiel-insert-date)

;; http://whattheemacsd.com/editing-defuns.el-01.html
(defun eqyiel-open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'eqyiel-open-line-below)

(defun eqyiel-open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "C-S-o") 'eqyiel-open-line-above)

;; http://whattheemacsd.com/setup-shell.el-01.html

;; C-d on an empty line in the shell terminates the process.
(defun eqyiel-comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;; With this snippet, another press of C-d kills the buffer.
;; Tap twice and be on your merry way!
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'eqyiel-comint-delchar-or-eof-or-kill-buffer)))

;; Also works to quickly dismiss a Python shell.
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (define-key inferior-python-mode-map
              (kbd "C-d") 'eqyiel-comint-delchar-or-eof-or-kill-buffer)))

;; http://whattheemacsd.com/buffer-defuns.el-02.html

(defun eqyiel-rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key (kbd "H-<return>") 'eqyiel-rotate-windows)

;; http://whattheemacsd.com/buffer-defuns.el-03.html

(defun eqyiel-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "H-SPC") 'eqyiel-toggle-window-split)

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

(defun eqyiel-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun eqyiel-duckduckgo ()
  "DDG a query or region if any."
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (url-hexify-string
            (if (use-region-p)
                (buffer-substring (region-beginning) (region-end))
              (read-string "DuckDuckGo: "))))))

(defun eqyiel-copy-file-name-to-clipboard ()
  "Copy the current `buffer-file-name' to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(global-set-key (kbd "C-c w") 'eqyiel-copy-file-name-to-clipboard)

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun eqyiel-rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'eqyiel-rename-file-and-buffer)

;; http://whattheemacsd.com/file-defuns.el-02.html
(defun eqyiel-delete-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'eqyiel-delete-file-and-buffer)

;; http://whattheemacsd.com/setup-dired.el-02.html
(defun eqyiel-dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(eval-after-load "dired"
  '(define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'eqyiel-dired-back-to-top))

(defun eqyiel-dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load "dired"
  '(define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'eqyiel-dired-jump-to-bottom))

(defun eqyiel-sprunge-region ()
  "Send the current region to sprunge.us and save the URL returned to the
kill-ring."
  (interactive)
  (if (use-region-p)
      (let ((filename (substring (shell-command-to-string "mktemp") 0 -1)))
        (write-region (region-beginning) (region-end) filename)
        (let ((url (substring (shell-command-to-string
                               (concat "curl -s -F 'sprunge=<"
                                       filename "' http://sprunge.us")) 0 -1)))
          (shell-command (concat "rm " filename))
          (kill-new url)
          (message "Saved %s to kill ring." url)))
    (message "Mark a region first.")))

(defun eqyiel-rotn-region (n)
  "Decode a caesar cipher.  Adapted from `rot13' to shift by N."
  (interactive "NHow many? ")
  (if (use-region-p)
      (let ((rotn-translate-table
             (let ((str (make-string 127 0))
                   (i 0))
               (while (< i 127)
                 (aset str i i)
                 (setq i (1+ i)))
               (setq i 0)
               (while (< i 26)
                 (aset str (+ i ?a) (+ (% (+ i n) 26) ?a))
                 (aset str (+ i ?A) (+ (% (+ i n) 26) ?A))
                 (setq i (1+ i)))
               str)))
      (translate-region (region-beginning) (region-end) rotn-translate-table))
    (message "Mark a region first.")))

(defun eqyiel-print-to-pdf (dest)
  "Pretty-print a buffer using PostScript and save it as a PDF."
  (interactive "FSave to where? ")
  (let ((tmp (substring (shell-command-to-string "mktemp") 0 -1)))
    (ps-spool-buffer-with-faces)
    (switch-to-buffer "*PostScript*")
    (write-file tmp)
    (kill-buffer (file-name-nondirectory tmp))
    (shell-command (concat "ps2pdf14 " tmp " " dest))
    (shell-command (concat "rm " tmp))
    (message (concat "PDF written to " dest "."))))

;; http://www.emacswiki.org/emacs/EmacsAsDaemon#toc9
(defun eqyiel-server-shutdown ()
 "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun eqyiel-count-commas ()
  "CSV files are a pain to read, use this to see if there are as many commas as
there should be."
  (interactive)
  (let ((i 0))
    (beginning-of-line)
    (while (re-search-forward "," (line-end-position) t)
      (setq i (+ i 1)))
    (message "found %s" i)))

(defun eqyiel-copy-rectangle-to-kill-ring (start end)
  "Saves a rectangle to the normal kill ring."
  (interactive "r")
  (let ((lines (extract-rectangle start end)))
    (with-temp-buffer
      (while lines
        (insert-for-yank (car lines))
        (insert "\n")
        (setq lines (cdr lines)))
      (kill-ring-save (point-min) (point-max)))))

(defun eqyiel-parent-directory (dir)
  (file-name-directory
      (directory-file-name
       dir)))

(provide 'eqyiel-lib)
