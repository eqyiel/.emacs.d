;;; eqyiel-calfw.el

(require 'calfw)
(require 'calfw-org)
(require 'calendar)
;; (setq  number-of-diary-entries 31) ;; the fuck is this
(define-key calendar-mode-map "f" 'calendar-forward-day)
(define-key calendar-mode-map "n" 'calendar-forward-day)
(define-key calendar-mode-map "b" 'calendar-backward-day)
(setq calendar-mark-holidays-flag t)
;;(require 'japanese-holidays) ;; breaks layout
(setq calendar-holidays
      (append general-holidays solar-holidays oriental-holidays))

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "#8fb28f")  ; orgmode source
    )))

(custom-set-faces
 '(cfw:face-annotation ((t :foreground "#dcdccc" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :foreground "#dcdccc" :background "#3f3f3f")))
 '(cfw:face-default-content ((t :foreground "#8fb28f")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "#dcdccc" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "#2b2b2b")))
 '(cfw:face-header ((t (:foreground "#dcdccc" :background "#3f3f3f" :weight bold))))
 '(cfw:face-holiday ((t :background "#3f3f3f" :foreground "#dcdccc" :weight bold)))
 '(cfw:face-periods ((t :foreground "#dcdccc")))
 '(cfw:face-saturday ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
 '(cfw:face-select ((t :background "#8fb28f" :foreground "#2b2b2b")))
 '(cfw:face-sunday ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
 '(cfw:face-title ((t (:foreground "#2b2b2b" :background "#8fb28f" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
 '(cfw:face-today-title ((t :background "#2b2b2b" :foreground "#8fb28f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "#8fb28f" :background "#2b2b2b")))
 '(cfw:face-toolbar-button-off ((t :foreground "#8fb28f" :background "#2b2b2b" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "#2b2b2b" :background "#8fb28f" :weight bold))))

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; ;; Default setting
;; (setq cfw:fchar-junction ?+
;;       cfw:fchar-vertical-line ?|
;;       cfw:fchar-horizontal-line ?-
;;       cfw:fchar-left-junction ?+
;;       cfw:fchar-right-junction ?+
;;       cfw:fchar-top-junction ?+
;;       cfw:fchar-top-left-corner ?+
;;       cfw:fchar-top-right-corner ?+ )

(provide 'eqyiel-calfw)
