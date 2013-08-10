;;; eqyiel-slime.el

;; requires js2-mode by yegge

(add-to-list 'load-path "~/.emacs.d/elpa/js2-mode-20121003.531")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq slime-repl-history-file "~/.emacs.d/cache/slime-history.eld")

;; absolutely requires these versions (found in marmalade):
;; slime-20100404.1
;; slime-js-0.0.1
;; slime-repl-20100404

(add-to-list 'load-path "~/.emacs.d/elpa/slime-20100404.1/")
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'setup-slime-js)
(require 'slime-repl)
(slime-setup '(slime-js slime-repl))

(setq slime-js-swank-command "swank-js")
(setq slime-js-swank-args '())
(global-set-key [f5] 'slime-js-reload)

(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))


;;; todo: could this file not be turned into a web-modes type of thing?
;;; css-mode hook for rainbow mode is already in eqyiel-misc.el.


;; http://julien.danjou.info/projects/emacs-packages#rainbow-mode

(require 'rainbow-mode)

(add-hook 'html-mode-hook (lambda () (rainbow-turn-on)))

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-turn-on)
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

(provide 'eqyiel-slime)
