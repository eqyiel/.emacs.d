;;; eqyiel-ido.el

(require 'ido)
(require 'ido-ubiquitous)

(defun eqyiel-ido-vertical-define-keys ()
  "C-n/p may be more intuitive in a vertical layout, but that doesn't mean you
can steal M-p from smex!!"
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(with-library ido-vertical-mode
  (turn-on-ido-vertical)
  (remove-hook 'ido-setup-hook 'ido-vertical-define-keys)
  (add-hook 'ido-setup-hook 'eqyiel-ido-vertical-define-keys))

;; if tramp is loaded
(add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)

;; http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

(setq
 ido-save-directory-list-file "~/.cache/emacs/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/doc" "~/dev" "~/.emacs.d/")
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-max-work-directory-list 30
 ido-max-work-file-list 50
 ido-use-filename-at-point nil
 ido-enable-flex-matching t
 ido-max-prospects 10
 ido-confirm-unique-completion t
 ido-use-virtual-buffers t
 ido-create-new-buffer 'always
 ido-auto-merge-work-directories-length nil)

;; don't ask, just do
(setq confirm-nonexistent-file-or-buffer nil)

(ido-mode 'both) ;; for buffers and files
(ido-ubiquitous-mode t)
(ido-everywhere t)

(provide 'eqyiel-ido)
