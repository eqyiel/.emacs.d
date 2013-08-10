;;; eqyiel-guide-key.el
;; Guide key is like having training wheels for a major mode.

;;; https://github.com/m2ym/popwin-el
;; required by guide-key

(require 'popwin)
(require 'diminish)
(popwin-mode 1)

;;; https://github.com/kbkbkbkb1/guide-key

(require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
;; (setq guide-key/guide-key-highlight-command-regexp "rectangle")

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(defun guide-key/my-hook-function-for-gnus-summary-mode ()
  (guide-key/add-local-guide-key-sequence "B")
  (guide-key/add-local-guide-key-sequence "K")
  (guide-key/add-local-guide-key-sequence "O")
  (guide-key/add-local-guide-key-sequence "G")
  (guide-key/add-local-guide-key-sequence "A")
  (guide-key/add-local-guide-key-sequence "S")
  (guide-key/add-local-guide-key-sequence "X")
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "gnus-summary-"))
(add-hook 'gnus-summary-mode-hook 'guide-key/my-hook-function-for-gnus-summary-mode)

(defun guide-key/my-hook-function-for-gnus-group-mode ()
  (guide-key/add-local-guide-key-sequence "T")
  (guide-key/add-local-guide-key-sequence "G")
  (guide-key/add-local-guide-key-sequence "M")
  (guide-key/add-local-guide-key-sequence "A")
  (guide-key/add-local-guide-key-sequence "C-c")
  ;; (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "gnus-group-"))
(add-hook 'gnus-group-mode-hook 'guide-key/my-hook-function-for-gnus-group-mode)

(defun guide-key/my-hook-function-for-gnus-article-mode ()
  (guide-key/add-local-guide-key-sequence "W")
  (guide-key/add-local-guide-key-sequence "S")
  ;; (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "gnus-article-"))
(add-hook 'gnus-article-mode-hook 'guide-key/my-hook-function-for-gnus-article-mode)

(defun guide-key/my-hook-function-for-message-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-m")
  ;; (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "message-")
  (guide-key/add-local-highlight-command-regexp "mml-secure"))
(add-hook 'message-mode-hook 'guide-key/my-hook-function-for-message-mode)

;;; Cool, but a bit too much
;; (setq guide-key/guide-key-sequence '("C-x")
;;       guide-key/recursive-key-sequence-flag t)

(guide-key-mode 1)

(diminish 'guide-key-mode)

(provide 'eqyiel-guide-key)
