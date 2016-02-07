;;; eqyiel-processing.el

(eval-after-load "processing-mode"
  '(setq processing-application-dir "/usr/share/processing"
         processing-location "/usr/bin/processing-java"))

(eval-after-load "processing-mode"
  '(define-key processing-mode-map (kbd "C-c C-c") 'processing-sketch-run))

(eval-after-load "processing-mode"
  '(add-to-list 'company-keywords-alist
                (cons 'processing-mode
                      (append '() processing-functions
                              processing-builtins
                              processing-constants))))

(provide 'eqyiel-processing)
