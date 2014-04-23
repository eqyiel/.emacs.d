;;; eqyiel-defaults.el

(set-face-attribute 'default nil :height 120 :family "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :height 100 :family "DejaVu Sans Mono")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(blink-cursor-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(global-subword-mode 1)


(setq-default
 indent-tabs-mode nil
 fill-column 80
 tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(setq
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix)

(set-locale-environment "en_US.UTF-8")

(setq browse-url-browser-function 'browse-url-generic)

(setq browse-url-generic-program "firefox")
;; (if (executable-find "conkeror")
;;     (setq browse-url-generic-program "conkeror")
;;   (setq browse-url-generic-program "firefox"))

;; don't litter
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.cache/emacs/backup"))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      echo-keystrokes 0.1
      auto-save-list-file-prefix "~/.cache/emacs/backup/.saves-")

;; I mean it!
(setq bookmark-default-file "~/.cache/emacs/emacs.bmk"
      url-cache-directory "~/.cache/emacs/url/cache")

(setq
 apropos-do-all t
 inhibit-startup-message t
 ediff-window-setup-function 'ediff-setup-windows-plain
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 scroll-error-top-bottom t
 scroll-preserve-screen-position 1
 scroll-step 1
 save-interprogram-paste-before-kill t
 frame-title-format '(buffer-file-name "%f" ("%b"))
 x-select-enable-clipboard t
 x-select-enable-primary t
 x-stretch-cursor t)

;; auto refresh buffers
(global-auto-revert-mode)
;; also auto refresh dired, but be quiet about it please
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; highlight isearch matches even when not in isearch, use
;; lazy-highlight-cleanup to clean up
(setq lazy-highlight-cleanup nil)

(require 'tramp)
(setq tramp-persistency-file-name "~/.cache/emacs/tramp"
      tramp-auto-save-directory "~/.cache/emacs/backup"
      tramp-default-method "scp"
      ;; tramp-debug-buffer t
      ;; tramp-verbose 10
      )

;; when tramp successfully logs in but hangs, it's probably because
;; tramp-terminal-prompt-regexp doesn't recognise it:
;; http://stackoverflow.com/a/8363532

;; note: uses sudo password, not root password.  no need to allow ssh for root.
(add-to-list 'tramp-default-proxies-alist '(".*" "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '("\\`localhost\\'" "\\`root\\'" nil))

(require 'winner)
(winner-mode t)

(require 'saveplace)
(setq save-place-file "~/.cache/emacs/saveplace")
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(setq recentf-save-file "~/.cache/emacs/.recentf"
      recentf-max-saved-items 1000)
(recentf-mode 1)

(require 'savehist)
(setq savehist-file "~/.cache/emacs/history"
      history-length 1000)
(savehist-mode 1)

;; (require 'package)
;; (setq package-user-dir "~/.emacs.d/site-lisp/elpa")
;; (package-initialize)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; http://stackoverflow.com/a/6830894/2204400
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p
                            (format "Directory %s does not exist. Create it?"
                                    dir)))
                  (make-directory dir t))))))


;; http://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer#toc1
;; Reuse same dired buffer when doing dired-up-directory
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; http://stackoverflow.com/a/12958498/2204400
;; Makefile headaches.
;; Try this if the defadvice doesn't work out.  https://github.com/glasserc/ethan-wspace

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
                                      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; let me use S-SPC to scroll backwards in info mode
(eval-after-load "info"
  '(define-key Info-mode-map (kbd "S-SPC") 'Info-scroll-down))

(add-hook 'Info-mode-hook
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)))

(eval-after-load 'kkc
  (setq kkc-init-file-name "~/.cache/emacs/kkcrc"))

(setq eshell-directory-name "~/.cache/emacs/eshell")

;; (eval-after-load 'eshell-mode
;;   '(progn (setq eshell-directory-name "~/.cache/emacs/eshell")))

(setq ispell-dictionary "en_GB")

(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)

;; don't accidentally tap suspend-frame
(global-unset-key [(control x)(control z)])
(global-unset-key [(control z)])

(provide 'eqyiel-defaults)
