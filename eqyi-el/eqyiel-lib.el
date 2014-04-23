;; eqyiel-lib.el
;; Most of these are taken from other people.  Where possible, a link to the
;; original author precedes the function.


;; http://www.emacswiki.org/emacs/LoadingLispFiles

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)
     (error (message (format "I guess we don't have %s available." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

;; http://david.rothlis.net/emacs/ergonomics.html

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; https://github.com/technomancy/emacs-starter-kit/

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'local-comment-auto-fill)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun turn-on-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (set (make-local-variable 'paredit-space-delimiter-chars)
       (list ?\"))
  (paredit-mode 1))

;; http://www.emacswiki.org/emacs/FullScreen#toc22

(defun toggle-fullscreen ()
  "Depends on community/wmctrl"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key [f11] 'toggle-fullscreen)

;; http://whattheemacsd.com/editing-defuns.el-01.html

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;; http://whattheemacsd.com/setup-shell.el-01.html

;; C-d on an empty line in the shell terminates the process.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))
;; with this snippet, another press of C-d kills the buffer.
;; tap twice and be on your merry way!
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))
;; Also works to quickly dismiss a python shell
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (define-key inferior-python-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; http://stackoverflow.com/a/14992483/2204400

(defun my-toggle-fill-paragraph ()
  ;; Based on http://xahlee.org/emacs/modernization_fill-paragraph.html
  "Fill or unfill the current paragraph, depending upon the current line length.
When there is a text selection, act on the region.
See `fill-paragraph' and `fill-region'."
  (interactive)
  ;; We set a property 'currently-filled-p on this command's symbol
  ;; (i.e. on 'my-toggle-fill-paragraph), thus avoiding the need to
  ;; create a variable for remembering the current fill state.
  (save-excursion
    (let* ((deactivate-mark nil)
           (line-length (- (line-end-position) (line-beginning-position)))
           (currently-filled (if (eq last-command this-command)
                                 (get this-command 'currently-filled-p)
                               (< line-length fill-column)))
           (fill-column (if currently-filled
                            most-positive-fixnum
                          fill-column)))

      (if (region-active-p)
          (fill-region (region-beginning) (region-end))
        (fill-paragraph))
      (put this-command 'currently-filled-p (not currently-filled)))))

;; http://whattheemacsd.com/buffer-defuns.el-02.html

(defun rotate-windows ()
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

(global-set-key (kbd "H-<return>") 'rotate-windows)

;; http://whattheemacsd.com/buffer-defuns.el-03.html

(defun toggle-window-split ()
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



;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

(defun open-with ()
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

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun duckduckgo ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "DuckDuckGo: "))))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; http://whattheemacsd.com/file-defuns.el-01.html

(defun rename-file-and-buffer ()
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

(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; http://whattheemacsd.com/file-defuns.el-02.html

(defun delete-file-and-buffer ()
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

(global-set-key (kbd "C-x C-k") 'delete-file-and-buffer)

;; http://whattheemacsd.com/setup-dired.el-02.html

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(eval-after-load "dired"
  '(define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'dired-back-to-top))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load "dired"
  '(define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'dired-jump-to-bottom))

; sprunge.us owns
(defun sprunge (prefix)
  "Posts the current buffer to sprunge, and shows the resulting URL in a new
buffer.  If invoked with the universal argument / prefix, upload the whole file,
else upload just the region."
  (interactive "P")
  (let ((filename "/tmp/sprunge-post"))
    (if prefix (write-file filename)
      (write-region (region-beginning) (region-end) filename))
    (insert (shell-command-to-string
             (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")))
    (delete-char -1))) ; Newline after URL

;; http://www.emacswiki.org/emacs/EmacsAsDaemon#toc9

(defun server-shutdown ()
 "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'eqyiel-lib)
