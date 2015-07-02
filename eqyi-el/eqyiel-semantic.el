;;; eqyiel-semantic.el

(require 'semantic)
(require 'semantic/db-javap)

(add-to-list
 'semantic-default-submodes
 '(global-semanticdb-minor-mode
   global-semantic-mru-bookmark-mode
   global-cedet-m3-minor-mode
   global-semantic-highlight-func-mode
   global-semantic-stickyfunc-mode
   global-semantic-decoration-mode
   global-semantic-idle-local-symbol-highlight-mode
   global-semantic-idle-scheduler-mode
   global-semantic-idle-completions-mode
   global-semantic-idle-summary-mode))

(provide 'eqyiel-semantic)
