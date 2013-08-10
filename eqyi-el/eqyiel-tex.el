;;; eqyiel-tex.el

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(autoload 'LaTeX-mode "LaTeX-mode" "LaTeX Mode." t)
(autoload 'LaTeX-mode-map "LaTeX-mode-map" "LaTeX Mode Map." t)

(setq
 TeX-PDF-mode t
 preview-default-document-pt 14.0
 preview-scale-function 3
 TeX-view-program-selection (quote
                             (((output-dvi style-pstricks) "dvips and gv")
                              (output-dvi "xdvi")
                              (output-pdf "xdg-open")
                              (output-html "xdg-open"))))

;; By default this is brings up the info page.  Annoying!
(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-c TAB") 'yas-expand))

(require 'auto-complete-auctex)
