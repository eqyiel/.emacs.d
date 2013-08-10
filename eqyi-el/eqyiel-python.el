;;; eqyiel-python.el

(setenv "PYMACS_PYTHON" "python2")
(setenv "PYTHONPATH" "${PYTHONPATH}:~/.local/lib/python2.7")

(require 'elpy)

(elpy-enable t) ;; passing t skips elpy-initialize-variables.
                ;; we want this because otherwise it overrides stuff.

(setq flymake-no-changes-timeout 60
      flymake-start-syntax-check-on-newline nil)


(setq elpy-rpc-python-command "python2"
      python-shell-interpreter "python2"
      python-check-command "pyflakes"
      elpy-set-backend "rope")

(defun turn-on-electric-pair-mode ()
  (electric-pair-mode t))
(add-hook 'python-mode-hook 'turn-on-electric-pair-mode)

(provide 'eqyiel-python)
