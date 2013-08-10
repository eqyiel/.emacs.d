;;; eqyiel-emms.el

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp/")
(require 'emms-setup)
(require 'emms-info-libtag)
(setq emms-directory "~/.emacs.d/cache/emms")
(setq emms-history-file "~/.emacs.d/cache/emms/history")
(setq emms-score-file "~/.emacs.d/cache/emms/scores")
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
;;(setq emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions '(emms-info-mpd))

(setq emms-player-mpd-music-directory "~/media/music/lib")
(setq emms-player-list (quote (emms-player-mpd emms-player-mpg321 emms-player-ogg123 emms-player-mplayer-playlist emms-player-mplayer emms-player-vlc)))
(setq
 emms-info-asynchronously t
 later-do-interval 0.0001
 emms-source-file-default-directory "~/media/music/lib"
 emms-mode-line-format " %s "
 emms-show-format "NP: %s")
;; emms-info-functions '(emms-info-libtag)

(emms-devel)
;;(emms-standard)
;;(emms-default-players)
(require 'emms-mark)
;;(setq emms-playlist-default-major-mode 'emms-mark-mode)

;; set up a default cover
(setq emms-browser-default-covers
      (list "/home/eqyiel/.emacs.d/cache/emms/cover_small.jpg" "/home/eqyiel/.emacs.d/cache/emms/cover_med.jpg" nil))

;; mpc + dzen2 does this better
;; (global-set-key (kbd "<C-right>") 'emms-seek-forward)
;; (global-set-key (kbd "<C-left>") 'emms-seek-backward)
(emms-player-mpd-connect)

(emms-browser-make-filter "not-played"
 (lambda (track) (not (funcall (emms-browser-filter-only-recent 365) track))))
(emms-browser-make-filter "last-3months" (emms-browser-filter-only-recent 90))
(emms-browser-make-filter "last-month" (emms-browser-filter-only-recent 30))
(emms-browser-make-filter "last-week" (emms-browser-filter-only-recent 7))
;;(emms-browser-make-filter "Compilation" (emms-browser-filter-only-dir "~/Mp3s/classical"))
(emms-browser-set-filter (assoc "all" emms-browser-filters))
(emms-browser-make-filter
 "all"
 (lambda (track)
   (or
    (funcall (emms-browser-filter-only-type 'file) track))))

;; HELP
;; (emms-browser-make-filter
;;  "discnumber"
;;  (lambda (track)
;;    (or
;;     (funcall (emms-browser-disc-number 'file) track))))


(add-hook 'emms-playlist-selection-changed-hook 'de-focus-on-track)
;; recenter based on the current track
(defun de-focus-on-track ()
  (let ((w (get-buffer-window emms-playlist-buffer t)))
    (when w
      (with-selected-window w
        (emms-playlist-mode-center-current)
        (recenter '(4))))))

(emms-mode-line-disable)
(emms-playing-time-disable-display)

 ;; '(emms-cache-get-function (quote emms-cache-get))
 ;; '(emms-cache-modified-function (quote emms-cache-dirty))
 ;; '(emms-cache-set-function (quote emms-cache-set))
 ;; '(emms-directory "~/.emacs.d/cache/emms")
 ;; '(emms-mode-line-format " %s ")
 ;; '(emms-mode-line-mode-line-function (quote emms-mode-line-icon-function))
 ;; '(emms-player-mpd-music-directory "~/media/music/lib")
 ;; '(emms-player-next-function (quote emms-score-next-noerror))
 ;; '(emms-playlist-default-major-mode (quote emms-playlist-mode))
 ;; '(emms-playlist-update-track-function (quote emms-playlist-mode-update-track-function))
 ;; '(emms-show-format "NP: %s")
 ;; '(emms-source-file-default-directory "~/media/music/lib")
 ;; '(emms-track-description-function (quote emms-info-track-description))

(provide 'eqyiel-emms)
