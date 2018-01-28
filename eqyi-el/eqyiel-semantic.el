;;; eqyiel-semantic.el

(require 'semantic)
(add-hook 'java-mode-hook (lambda () (require 'semantic/db-javap)))

(add-to-list
 'semantic-default-submodes
 '(global-semanticdb-minor-mode
   global-semantic-mru-bookmark-mode
   global-cedet-m3-minor-mode
   global-semantic-highlight-func-mode
   global-semantic-decoration-mode
   global-semantic-idle-local-symbol-highlight-mode
   global-semantic-idle-scheduler-mode
   global-semantic-idle-completions-mode
   global-semantic-idle-summary-mode))

(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump) ;; Jump to definition

(dolist (hook '(c-mode-hook
                c++-mode-hook
                emacs-lisp-mode-hook
                java-mode-hook
                js-mode-hook
                scheme-mode-hook))
  (add-hook hook 'semantic-mode))

(provide 'eqyiel-semantic)
