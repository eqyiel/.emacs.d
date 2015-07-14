;;; eqyiel-ibuffer.el

(autoload 'ibuffer "ibuffer")

(eval-after-load "ibuffer"
  '(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Mail"
                (or  ;; mail-related buffers
                 (mode . message-mode)
                 (mode . mail-mode)
                 (mode . gnus-summary-mode)
                 (mode . gnus-article-mode)
                 (mode . gnus-group-mode)))
               ("~/.emacs.d/"
                (filename . ".emacs.d/"))
               ("Programming"
                 (or
                  (mode . prog-mode)
                  (mode . comint-mode)))
               ("IRC"
                (mode . circe-channel-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'eqyiel-ibuffer)
