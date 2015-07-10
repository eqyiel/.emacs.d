;;; eqyiel-python.el

(setenv "PYTHONPATH" "${PYTHONPATH}:~/.emacs.d/site-lisp/elpy/")
(autoload 'elpy-enable "elpy" nil t)
(autoload 'pyvenv-activate "pyvenv-activate" nil t)
(autoload 'pyvenv "pyvenv-activate" nil t)

(eval-after-load "elpy"
  '(setq elpy-rpc-backend "jedi"
         elpy-rpc-python-command "python"
         flymake-no-changes-timeout 60
         flymake-start-syntax-check-on-newline nil
         python-check-command "pyflakes"
         python-shell-interpreter "python"))

(add-hook 'python-mode-hook (lambda () (elpy-enable)))

(provide 'eqyiel-python)
