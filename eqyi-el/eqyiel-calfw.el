;;; eqyiel-calfw.el

;; (require 'calfw)
;; (require 'calfw-org)
;; (require 'calendar)

(autoload 'cfw:open-calendar-buffer "calfw" nil t)
(autoload 'cfw:org-create-source "calfw-org" nil t)

;; (setq  number-of-diary-entries 31) ;; the fuck is this
;; (define-key calendar-mode-map "f" 'calendar-forward-day)
;; (define-key calendar-mode-map "n" 'calendar-forward-day)
;; (define-key calendar-mode-map "b" 'calendar-backward-day)

;; todo: only have the ones that are public holidays here
(setq australian-holidays
  '((holiday-fixed 01 01 "New Year's Day")
    (holiday-fixed 01 25 "Australia Day")
    (holiday-float 02 01 02 "Royal Hobart Regatta")
    (holiday-float 03 01 01 "Labour Day (WA)")
    (holiday-float 03 01 02 "Adelaide Cup (SA)")
    (holiday-float 03 01 02 "Canberra Day (ACT)")
    (holiday-float 03 01 02 "Eight Hours Day (TAS)")
    (holiday-float 03 01 02 "Labour Day (VIC)")
    (holiday-fixed 04 25 "ANZAC Day")
    (holiday-float 5 1 1 "May Day")
    (holiday-float 06 01 01 "Western Australia Day")
    (holiday-float 06 01 02 "Queen's Birthday (all states but WA)")
    (holiday-float 08 01 01 "Picnic Day")
    (holiday-float 10 01 01 "Labour Day (ACT, NSW, QLD, SA)")
    (holiday-float 11 01 01 "Recreation Day (TAS)")
    (holiday-float 11 02 01 "Melbourne Cup (VIC)")
    (holiday-fixed 12 24 "Christmas Eve")
    (holiday-fixed 12 25 "Christmas Day")
    (holiday-fixed 12 26 "Boxing Day")
    (holiday-fixed 12 31 "New Year's Eve")))

;;(require 'japanese-holidays) ;; breaks layout
(eval-after-load "calendar"
  '(setq calendar-mark-holidays-flag t
         calendar-holidays (append
                            australian-holidays
                            general-holidays
                            solar-holidays
                            oriental-holidays)))


      ;; calendar-holidays '((holiday-fixed 01 01 "Gesetzlicher Feiertag (Neujahr)")
      ;;                     (holiday-fixed 05 01 "Gesetzlicher Feiertag (Maifeiertag)")
      ;;                     (holiday-fixed 10 03 "Gesetzlicher Feiertag (Tag der Deutschen Einheit)")
      ;;                     (holiday-fixed 12 25 "Gesetzlicher Feiertag (1. Weihnachtstag)")
      ;;                     (holiday-fixed 12 26 "Gesetzlicher Feiertag (2. Weihnachtstag)")
      ;;                     (holiday-easter-etc -2 "Gesetzlicher Feiertag (Karfreitag)")
      ;;                     (holiday-easter-etc  1 "Gesetzlicher Feiertag (Ostermontag)")
      ;;                     (holiday-easter-etc 39 "Gesetzlicher Feiertag (Christi Himmelfahrt)")
      ;;                     (holiday-easter-etc 50 "Gesetzlicher Feiertag (Pfingstmontag)")))

;; (eval-after-load "calfw"
;;   '(progn
;;      (require 'calfw-org)
;;      (require 'calendar)))

(defun eqyiel-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source))))

;; (cfw:open-calendar-buffer
;;  :contents-sources
;;  (list (cfw:org-create-source "#8fb28f")))

;; (custom-set-faces
;;  '(cfw:face-annotation ((t :foreground "#dcdccc" :inherit cfw:face-day-title)))
;;  '(cfw:face-day-title ((t :foreground "#dcdccc" :background "#3f3f3f")))
;;  '(cfw:face-default-content ((t :foreground "#8fb28f")))
;;  '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
;;  '(cfw:face-disable ((t :foreground "#dcdccc" :inherit cfw:face-day-title)))
;;  '(cfw:face-grid ((t :foreground "#2b2b2b")))
;;  '(cfw:face-header ((t (:foreground "#dcdccc" :background "#3f3f3f" :weight bold))))
;;  '(cfw:face-holiday ((t :background "#3f3f3f" :foreground "#dcdccc" :weight bold)))
;;  '(cfw:face-periods ((t :foreground "#dcdccc")))
;;  '(cfw:face-saturday ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
;;  '(cfw:face-select ((t :background "#8fb28f" :foreground "#2b2b2b")))
;;  '(cfw:face-sunday ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
;;  '(cfw:face-title ((t (:foreground "#2b2b2b" :background "#8fb28f" :weight bold :height 2.0 :inherit variable-pitch))))
;;  '(cfw:face-today ((t :foreground "#dcdccc" :background "#3f3f3f" :weight bold)))
;;  '(cfw:face-today-title ((t :background "#2b2b2b" :foreground "#8fb28f" :weight bold)))
;;  '(cfw:face-toolbar ((t :foreground "#8fb28f" :background "#2b2b2b")))
;;  '(cfw:face-toolbar-button-off ((t :foreground "#8fb28f" :background "#2b2b2b" :weight bold)))
;;  '(cfw:face-toolbar-button-on ((t :foreground "#2b2b2b" :background "#8fb28f" :weight bold))))

(if (display-graphic-p)
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)
  (setq cfw:fchar-junction ?+
        cfw:fchar-vertical-line ?|
        cfw:fchar-horizontal-line ?-
        cfw:fchar-left-junction ?+
        cfw:fchar-right-junction ?+
        cfw:fchar-top-junction ?+
        cfw:fchar-top-left-corner ?+
        cfw:fchar-top-right-corner ?+))

(provide 'eqyiel-calfw)
