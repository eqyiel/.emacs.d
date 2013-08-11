;;; eqyiel-slime.el

;; requires js2-mode by yegge
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system

(require 'slime)
(require 'slime-js)
(require 'slime-repl)

;; http://julien.danjou.info/projects/emacs-packages#rainbow-mode
(require 'rainbow-mode)

(add-hook 'html-mode-hook (lambda () (rainbow-turn-on)))

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-turn-on)
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

;; https://github.com/magnars/.emacs.d
;; thanks magnars!! ( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ

(setq slime-repl-history-file "~/.cache/emacs/slime-history.eld")

;; python2 -m SimpleHTTPServer 8090
(setq slime-js-target-url "http://localhost:8090")
(setq slime-js-connect-url "http://localhost:8009")
(setq slime-js-starting-url "/")
(setq slime-js-swank-command "swank-js")
(setq slime-js-swank-args '("4008"))
(setq slime-js-browser-command "")
(setq slime-js-browser-jacked-in-p nil)

(add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))

(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (apply #'make-comint "swank-js" slime-js-swank-command nil slime-js-swank-args))

(defun slime-js-jack-in-node ()
  "Start a swank-js server and connect to it, opening a repl."
  (interactive)
  (slime-js-run-swank)
  (sleep-for 1)
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4008))

(defun slime-js-jack-in-browser ()
  "Start a swank-js server, connect to it, open a repl, open a browser, connect
   to that."
  (interactive)
  (slime-js-jack-in-node)
  (sleep-for 2)
  (slime-js-set-target-url slime-js-target-url)
  (shell-command (concat slime-js-browser-command " "
                         slime-js-connect-url slime-js-starting-url))
  (sleep-for 3)
  (setq slime-remote-history nil)
  (slime-js-sticky-select-remote (caadr (slime-eval '(js:list-remotes))))
  (setq slime-js-browser-jacked-in-p t)
  (global-set-key [f5] 'slime-js-reload))

;; error like this mean that auto-complete is interfering.
; pipelined request... (swank:interactive-eval "SwankJS.refreshCSS('main.css')")
(defadvice save-buffer (after save-css-buffer activate)
  (when (and slime-js-browser-jacked-in-p (eq major-mode 'css-mode))
    (slime-js-refresh-css)))

(defun js2-eval-friendly-node-p (n)
  (or (and (js2-stmt-node-p n) (not (js2-block-node-p n)))
      (and (js2-function-node-p n) (js2-function-node-name n))))

(defun slime-js--echo-result (result &rest _)
  (message result))

(defun slime-js--replace-with-result (replacement beg end)
  (save-excursion
    (goto-char beg)
    (delete-char (- end beg))
    (insert replacement)))

;; TODO: add in func
(defun slime-js-eval-buffer ()
(slime-js-eval-region (point-min) (point-max)))

(defun slime-js-eval-region (beg end &optional func)
  (lexical-let ((func (or func 'slime-js--echo-result))
                (beg beg)
                (end end))
    (slime-flash-region beg end)
    (slime-js-eval
     (buffer-substring-no-properties beg end)
     #'(lambda (s) (funcall func (cadr s) beg end)))))

(defun slime-js-eval-statement (&optional func)
  (let ((node (js2r--closest 'js2-eval-friendly-node-p)))
    (slime-js-eval-region (js2-node-abs-pos node)
                          (js2-node-abs-end node)
                          func)))

(defun slime-js-eval-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark))
    (slime-js-eval-statement)))

(defun slime-js-eval-and-replace-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark) 'slime-js--replace-with-result)
    (slime-js-eval-statement 'slime-js--replace-with-result)))

(define-key slime-js-minor-mode-map (kbd "C-x C-e")
  'slime-js-eval-current)
(define-key slime-js-minor-mode-map (kbd "C-c C-e")
  'slime-js-eval-and-replace-current)

;; Remove slime-minor-mode from mode line if diminish.el is installed
(when (boundp 'diminish)
  (diminish 'slime-js-minor-mode))

(provide 'eqyiel-slime)
