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

;; (comint-send-string "*nodejs*" "var c = require(`./ColorUtils.js')")

(provide 'eqyiel-javascript)
