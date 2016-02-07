;;; eqyiel-calfw.el

(autoload 'cfw:open-calendar-buffer "calfw" nil t)
(autoload 'cfw:org-create-source "calfw-org" nil t)

(setq eqyiel-australian-holidays
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

(eval-after-load "calendar"
  '(setq calendar-mark-holidays-flag t
         calendar-holidays (append
                            eqyiel-australian-holidays)))

(defun eqyiel-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source))))

(provide 'eqyiel-calfw)
