;;; init.el

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(when (not (car (file-attributes "~/.cache/emacs")))
  (make-directory "~/.cache/emacs"))

(when (not (car (file-attributes "~/.local/share/emacs")))
  (make-directory "~/.local/share/emacs"))

(add-subdirs-to-load-path "~/.emacs.d")
(add-subdirs-to-load-path "~/.local/share/emacs")

(defmacro with-library (symbol &rest body)
  "See http://www.emacswiki.org/emacs/LoadingLispFiles"
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)
     (error (message (format "%s was not available." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)

(if (equal (system-name) "ayanami")
    (with-library solarized
      (setq solarized-scale-org-headlines nil
            solarized-use-variable-pitch nil)
      (load-theme 'solarized-light t)))

;; (with-library zenburn-theme
;;   (load-theme 'zenburn t))

;; (require 'eqyiel-cedet)
;; (require 'eqyiel-elpa)
(require 'eqyiel-custom-junk)
(require 'eqyiel-defaults)
(require 'eqyiel-lib)
;; (require 'eqyiel-company)
;; ;; (require 'eqyiel-org)
;; (require 'eqyiel-pass)
;; (require 'eqyiel-circe)
;; (require 'eqyiel-gnus)
;; (require 'eqyiel-ibuffer)
;; (require 'eqyiel-ido)
;; (require 'eqyiel-misc)
;; (require 'eqyiel-calfw)
;; (require 'eqyiel-python)
;; (require 'eqyiel-java)
;; (require 'eqyiel-tex)
;; (require 'eqyiel-javascript)
;; (require 'eqyiel-web)
;; (require 'eqyiel-android)
;; (require 'eqyiel-semantic)
;; (require 'eqyiel-scss)
;; (require 'eqyiel-flycheck)
;; (require 'eqyiel-cc)
;; (if (eq system-type 'darwin)
;;   (require 'eqyiel-osx))

;; `use-package'----------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/site-lisp/elpa")
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)


;; Bootstrap `use-package' and its dependencies if they are not already
;; available.
(let ((dependencies '(use-package diminish bind-key)))
  (unless (seq-reduce (lambda (prev next) (and prev next))
                      (mapcar 'package-installed-p dependencies) t)
    (package-refresh-contents)
    (dolist (package dependencies)
      (unless (package-installed-p package)
        (package-install package)))))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-defer t)

;; `swiper' and friends --------------------------------------------------------

(use-package smex
  :init (setq smex-save-file "~/.cache/emacs/smex-items")
  :demand)

(use-package counsel :demand)

(use-package ivy :demand :diminish ivy-mode)

(use-package swiper
  :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-s" . swiper) ;; replace isearch-forward
         ("C-r" . swiper) ;; replace isearch-backward
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ;; be more like `ido-mode' in the minibuffer
         :map ivy-minibuffer-map
         ("C-s" . ivy-next-line-or-history)
         ("C-r" . ivy-previous-line-or-history)
         :map swiper-map
         ("C-r" . ivy-previous-line-or-history)
         ("C-s" . ivy-next-line-or-history)
         :map counsel-find-file-map
         ("C-r" . ivy-previous-line)
         ("C-s" . ivy-next-line)
         :map counsel-git-grep-map
         ("C-r" . ivy-previous-line)
         ("C-s" . ivy-next-line)
         :map ivy-switch-buffer-map
         ("C-r" . ivy-previous-line)
         ("C-s" . ivy-next-line)))

;; `smartparens' ---------------------------------------------------------------

(defun eqyiel/get-derived-mode-parents (mode)
  (when (and mode (boundp 'derived-mode-parents))
    (cons mode (derived-mode-parents (get mode 'derived-mode-parent)))))

(use-package smartparens
  :init
  (advice-add
   'sp-splice-sexp-killing-around
   :before-until
   ;; Don't steal M-r in comint-mode or modes derived from comint-mode.
   (lambda (&rest whatever)
     (when (or (eq major-mode 'comint-mode)
               (member 'comint-mode
                       (eqyiel/get-derived-mode-parents major-mode)))
       (comint-history-isearch-backward-regexp))))
  (smartparens-global-mode 1)
  :config
  (sp-use-paredit-bindings)
  (setq sp-autoskip-closing-pair 'always
        sp-ignore-modes-list ;; Also be smart in the minibuffer.
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'org-mode "~" "~")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "_" "_")
  (sp-local-pair 'org-mode "/" "/")
  :demand
  :diminish smartparens-mode)

;; `multiple-cursors' ----------------------------------------------------------

(use-package multiple-cursors
  :init (setq mc/list-file "~/.cache/emacs/mc-lists.el")
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)))

;; `magit' ---------------------------------------------------------------------

(use-package magit :bind (("<f8>" . magit-status)))

;; `shell-script-mode' ---------------------------------------------------------

(defun eqyiel/setup-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(add-hook 'sh-mode-hook 'eqyiel/setup-sh-mode)

;; `matrix-client' -------------------------------------------------------------

(defun eqyiel/launch-matrix-client () (interactive) (matrix-client "eqyiel"))

(use-package matrix-client
  :init (setq matrix-homeserver-base-url "https://matrix.rkm.id.au"))

;; `json-mode' -----------------------------------------------------------------

(use-package json-mode)

;; `buffer-move' ---------------------------------------------------------------

(use-package buffer-move  
  :bind
  ("H-h" . windmove-left)
  ("H-j" . windmove-down)
  ("H-k" . windmove-up)
  ("H-l" . windmove-right)
  ("H-b" . shrink-window-horizontally)
  ("H-f" . enlarge-window-horizontally)
  ("H-n" . shrink-window)
  ("H-p" . enlarge-window)
  ("M-H-h" . buf-move-left)
  ("M-H-j" . buf-move-down)
  ("M-H-k" . buf-move-up)
  ("M-H-l" . buf-move-right)
  ;; Caps lock and Menu keys are bound to Hyper, except on OSX which apparently
  ;; can't into Hyper.  Use fake Hyper from Karabiner-elements instead, which is
  ;; really M-s-S-C.
  ;;
  ;; See:
  ;; http://www.tenshu.net/p/fake-hyper-key-for-osx.html
  ;; https://github.com/tekezo/Karabiner-Elements/pull/170
  ("M-s-S-C-h" . windmove-left)
  ("M-s-S-C-j" . windmove-down)
  ("M-s-S-C-k" . windmove-up)
  ("M-s-S-C-l" . windmove-right)
  ("M-s-S-C-b" . shrink-window-horizontally)
  ("M-s-S-C-f" . enlarge-window-horizontally)
  ("M-s-S-C-n" . shrink-window)
  ("M-s-S-C-p" . enlarge-window))

;; `expand-region` -------------------------------------------------------------

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; `key-chord' -----------------------------------------------------------------

(use-package key-chord
  :config
  (key-chord-mode t)
  (key-chord-define-global "jk" 'er/expand-region)
  (key-chord-define-global "kj" 'er/expand-region)
  (key-chord-define-global "jo" 'other-window)
  (key-chord-define-global "oj" 'other-window)
  :init (use-package expand-region)
  :demand)

;; `yasnippet' -----------------------------------------------------------------

(use-package yasnippet
  :config (yas-global-mode t)
  :init (setq yas-snippet-dirs '("~/.emacs.d/eqyi-el/snippets"
                                 yas-installed-snippets-dir)
              yas-prompt-functions '(yas-ido-prompt))
  :bind (("C-c TAB" . yas-expand))
  :diminish yas-minor-mode
  :demand)
