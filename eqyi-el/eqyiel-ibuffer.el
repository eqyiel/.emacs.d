;;; eqyiel-ibuffer.el

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Mail"
                (or  ;; mail-related buffers
                 (mode . message-mode)
                 (mode . mail-mode)))
               ("~/.emacs.d/"
                (filename . ".emacs.d/"))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (mode . java-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 (mode . haskell-mode)
                 (mode . html-mode)
                 (mode . css-mode)
                 (mode . js2-mode)))
               ("IRC"   (mode . circe-channel-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'eqyiel-ibuffer)
