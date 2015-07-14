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

(defun eqyiel-javascript-mode-hook ()
  (set (make-local-variable 'company-backends) '((company-tern
                                                  company-files
                                                  company-yasnippet))))

(add-hook 'js2-mode-hook 'eqyiel-javascript-mode-hook)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load "js2-mode"
  '(setq js2-highlight-level 3))

(provide 'eqyiel-javascript)
