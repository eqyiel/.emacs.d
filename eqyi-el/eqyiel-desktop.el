;;; eqyiel-desktop.el

;;Remember what files I was working on


(require 'desktop)

(setq desktop-path '("~/.cache/emacs/")
      desktop-dirname "~/.cache/emacs/"
      desktop-base-file-name "emacs-desktop"
      desktop-load-locked-desktop t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

(desktop-save-mode 1)

;; http://www.emacswiki.org/emacs/DeskTop#toc3
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'my-desktop-save)

(provide 'eqyiel-desktop)
