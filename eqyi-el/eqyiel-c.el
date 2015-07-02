;;; eqyiel-c.el

(require 'flymake)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)

;; get feedback from flymake
(setq help-at-pt-timer-delay 0.1
      help-at-pt-display-when-idle t)

(defun my-cc-mode-hook ()
  (progn (setq tab-width 2
               c-basic-offset 2)
         (if (file-exists-p "Makefile")
             (flymake-mode-on))))

(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width 8)))

(semantic-mode 1)

(provide 'eqyiel-c)
