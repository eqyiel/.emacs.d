;;; eqyiel-python.el

;; (setenv "PYMACS_PYTHON" "python2")
(setenv "PYTHONPATH" "${PYTHONPATH}:~/.emacs.d/site-lisp/elpy/:~/.local/lib/python2.7")

;; (require 'elpy)
(autoload 'elpy-enable "elpy" nil t)
(autoload 'pyvenv-activate "pyvenv-activate" nil t)
(autoload 'pyvenv "pyvenv-activate" nil t)

(eval-after-load "elpy"
  '(setq elpy-set-backend "rope"
         elpy-rpc-python-command "python2"
         flymake-no-changes-timeout 60
         flymake-start-syntax-check-on-newline nil
         python-check-command "pyflakes"
         python-shell-interpreter "python2"))

;; Passing t skips elpy-initialize-variables.  This is desirable because I
;; want to set up auto-complete myself.
(add-hook 'python-mode-hook (lambda () (elpy-enable t)))

(provide 'eqyiel-python)
