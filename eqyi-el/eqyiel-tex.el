;;; eqyiel-tex.el

;; todo: figure out how to indent this:
;; \usepackage[style=verbose-inote,
;; backend=biber]{biblatex}
;; like this:
;; \usepackage[style=verbose-inote,
;;   backend=biber]{biblatex}
;; or this:
;; \usepackage[style=verbose-inote,
;;             backend=biber]{biblatex}

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; fill properly when dealing with LaTeX comments and syntax
(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "M-q") 'LaTeX-fill-paragraph))

(eval-after-load "tex"
  '(setq ;; TeX-PDF-mode t
         TeX-parse-self t
         TeX-engine 'xetex
         TeX-view-program-selection
         (quote
          (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "xdg-open")
           (output-html "xdg-open")))))

(eval-after-load "latex"
  '(setq LaTeX-item-indent 0 ;; indent items correctly
         TeX-PDF-mode t))

;; (autoload 'latex-mode "latex-mode" nil t)
;; (autoload 'latex-mode-map "latex-mode-map" nil t)

;; ;; By default this is brings up the info page.  Annoying!
;; (eval-after-load "latex"
;;   '(define-key latex-mode-map (kbd "C-c TAB") 'yas-expand))

;; (if (fboundp 'fci-mode)
;;     '((add-hook 'latex-mode-hook 'turn-on-fci-mode)
;;       (add-hook 'tex-mode-hook 'turn-on-fci-mode)))

;; todo: make editing bibtex files nicer,
;; newline-and-indent doesn't do what it is supposed to do, and to align things
;; you have to type C-c C-q
;; 2 is a much better indent than 18
;; (setq bibtex-text-indentation 2)
;; (setq bibtex-contline-indentation 2)
;; (setq bibtex-field-indentation 2)

(provide 'eqyiel-tex)
