;;; eqyiel-tex.el

;; TODO: figure out how to indent this:
;; \usepackage[style=verbose-inote,
;; backend=biber]{biblatex}
;; like this:
;; \usepackage[style=verbose-inote,
;;   backend=biber]{biblatex}
;; or this:
;; \usepackage[style=verbose-inote,
;;             backend=biber]{biblatex}

;; TODO: make editing bibtex files nicer, `newline-and-indent' doesn't do what
;; it is supposed to do, and to align things you have to type C-c C-q
;; 2 is a much better indent than 18
;; (setq bibtex-text-indentation 2)
;; (setq bibtex-contline-indentation 2)
;; (setq bibtex-field-indentation 2)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Fill properly when dealing with LaTeX comments and syntax
(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "M-q") 'LaTeX-fill-paragraph))

(eval-after-load "tex"
  '(setq TeX-parse-self t
         TeX-engine 'xetex
         TeX-view-program-selection
         '((output-dvi "xdg-open")
           (output-pdf "xdg-open")
           (output-html "xdg-open"))))

(eval-after-load "latex"
  '(setq LaTeX-item-indent 0 ;; indent \item correctly
         TeX-PDF-mode t))

(add-hook 'latex-mode-hook (lambda (TeX-PDF-mode-on)))

(provide 'eqyiel-tex)
