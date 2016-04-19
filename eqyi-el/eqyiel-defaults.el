;;; eqyiel-defaults.el

(setq user-full-name "Ruben Maher"
      user-mail-address "r@rkm.id.au"
      mail-host-address "rkm.id.au")
(if (string-equal (system-name) "ayanami")
    (progn
      (set-face-attribute 'default nil :height 120 :family "DejaVu Sans Mono")
      (set-fontset-font "fontset-default" nil (font-spec :name "Noto Emoji")))
      (set-face-attribute 'default nil :height 120 :family "Monaco"))

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
 tab-width 2
 default-tab-width 2
 standard-indent 2
 require-final-newline t
 sentence-end-double-space t
 eval-expression-print-length nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Give me two spaces everywhere
(add-hook 'c-mode-common-hook #'(lambda () (setq c-basic-offset tab-width)))

(setq buffer-file-coding-system 'utf-8-unix
      default-file-name-coding-system 'utf-8-unix
      default-keyboard-coding-system 'utf-8-unix
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      default-sendmail-coding-system 'utf-8-unix
      default-terminal-coding-system 'utf-8-unix)

(set-locale-environment "en_US.UTF-8")

(if (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))

(eval-after-load "warnings"
  ;; Stop warning me about my load path including ~/.emacs.d.
  '(setq display-warning-minimum-level :error))

;; Stop nagging me to enable these useful commands.
(dolist (x '(dired-find-alternate-file
             upcase-region
             downcase-region))
  (put x 'disabled nil))

;; Don't litter my ~/.emacs.d.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.cache/emacs/backup"))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      echo-keystrokes 0.1
      auto-save-default nil
      auto-save-list-file-prefix "~/.cache/emacs/backup/.saves-"
      bookmark-default-file "~/.cache/emacs/emacs.bmk"
      url-cache-directory "~/.cache/emacs/url/cache"
      eshell-directory-name "~/.cache/emacs/eshell"
      custom-file "~/.emacs.d/eqyi-el/eqyiel-custom-junk.el")

;; No really.
(eval-after-load "kkc"
  '(setq kkc-init-file-name "~/.cache/emacs/kkcrc"))

(setq apropos-do-all t
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

;; Automagically refresh buffers if they are changed on disk.
(global-auto-revert-mode)
;; Same for directories, but be quiet about it please.
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Highlight isearch matches even when not in isearch, use
;; `lazy-highlight-cleanup' to clean up.
;; (setq lazy-highlight-cleanup nil)

(require 'tramp)
(setq tramp-persistency-file-name "~/.cache/emacs/tramp"
      tramp-auto-save-directory "~/.cache/emacs/backup"
      tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; Useful for debugging `tramp'
;; (setq tramp-debug-buffer t
;;       tramp-verbose 10)

;; When `tramp' successfully logs in but hangs, it's probably because
;; `tramp-terminal-prompt-regexp' doesn't recognise it:
;; http://stackoverflow.com/a/8363532

;; Uses sudo password for the user defined in ~/.ssh/config, not root password.
;; No need to allow ssh for root.
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
;; Reuse same dired buffer when doing `dired-up-directory'.
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))))

(eval-after-load "dired"
  '(progn
     (setq dired-dwim-target t
           dired-recursive-deletes 'top)
     ;; Let me use ido when renaming/moving files
     (put 'dired-do-rename 'ido 'find-file)
     (put 'dired-do-copy 'ido 'find-file)))

(setq delete-by-moving-to-trash t)

(global-set-key [f11] 'toggle-frame-fullscreen)

;; Let me use S-SPC to scroll backwards in info mode.
(eval-after-load "info"
  '(define-key Info-mode-map (kbd "S-SPC") 'Info-scroll-down))

(add-hook 'Info-mode-hook
          (lambda ()
            (setq Info-additional-directory-list Info-default-directory-list)))

(eval-after-load "ispell"
  '(progn
     (when (executable-find ispell-program-name)
       (setq ispell-dictionary "en_GB"
             ispell-personal-dictionary "~/.aspell.en.pws")
       (add-hook 'text-mode-hook 'turn-on-flyspell)
       (add-hook 'prog-mode-hook 'flyspell-prog-mode))
     (when (eq system-type 'darwin)
       (setenv "DICTIONARY" "en_GB"))))

(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)

;; Make isearch gooder
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Make it easy to write about keybindings
(global-set-key (kbd "C-c e") 'edmacro-insert-key)

;; Don't accidentally tap `suspend-frame'
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; Why don't you respect `tab-width?'
(eval-after-load "shell-script-mode"
  '(setq sh-make-vars-local nil
         sh-basic-offset 2
         sh-indentation 2))

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list '(("Asia/Tokyo" "Tokyo")
                                ("Australia/Adelaide" "Adelaide")
                                ("Australia/Brisbane" "Brisbane")
                                ("Australia/Canberra" "Canberra")
                                ("Australia/Darwin" "Darwin")
                                ("Australia/Hobart" "Hobart")
                                ("Australia/Melbourne" "Melbourne")
                                ("Australia/Perth" "Perth")
                                ("Australia/Sydney" "Sydney")))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width 8)))

;; Open systemd service files with an appropriate mode.
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))

;; Use `eldoc' in `emacs-lisp-mode' buffers.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(eval-after-load "help-at-pt"
  '(setq help-at-pt-timer-delay 0.1
         help-at-pt-display-when-idle t))

(setq find-function-C-source-directory "~/dev/emacs")

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(provide 'eqyiel-defaults)
