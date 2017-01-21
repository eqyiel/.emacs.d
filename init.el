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

(with-library solarized
  (setq solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t))

;; (with-library zenburn-theme
;;   (load-theme 'zenburn t))

;; (require 'eqyiel-cedet)
(require 'eqyiel-elpa)
(require 'eqyiel-custom-junk)
(require 'eqyiel-defaults)
(require 'eqyiel-lib)
(require 'eqyiel-company)
(require 'eqyiel-org)
(require 'eqyiel-pass)
(require 'eqyiel-circe)
(require 'eqyiel-gnus)
(require 'eqyiel-ibuffer)
(require 'eqyiel-ido)
(require 'eqyiel-misc)
(require 'eqyiel-calfw)
(require 'eqyiel-python)
(require 'eqyiel-java)
(require 'eqyiel-tex)
(require 'eqyiel-javascript)
(require 'eqyiel-web)
(require 'eqyiel-android)
(require 'eqyiel-semantic)
(require 'eqyiel-scss)
(require 'eqyiel-flycheck)
(require 'eqyiel-cc)
(if (eq system-type 'darwin)
  (require 'eqyiel-osx))

;; `use-package'----------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package' and its dependencies if they are not already
;; available.
(unless
    (seq-reduce
     (lambda (prev next) (and prev next))
     (mapcar
      'package-installed-p
      '(use-package diminish bind-key))
     t)
  (package-refresh-contents)
  (dolist (package eqyiel-package-list)
    (unless (package-installed-p package)
      (package-install package))))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; `smartparens' ---------------------------------------------------------------

(defun eqyiel/get-derived-mode-parents (mode)
  (when mode
    (cons mode (derived-mode-parents
                (get mode 'derived-mode-parent)))))

(use-package smartparens
  :init
  (progn
    (advice-add
     'sp-splice-sexp-killing-around
     :before-until
     ;; Don't steal M-r in comint-mode or modes derived from comint-mode.
     (lambda (&rest whatever)
       (when (or (eq major-mode 'comint-mode)
                 (member 'comint-mode
                         (eqyiel/get-derived-mode-parents major-mode)))
         (comint-history-isearch-backward-regexp))))
    (use-package smartparens-config)
    (smartparens-global-mode 1))
  :config
  (progn
    (sp-use-paredit-bindings)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-ignore-modes-list  ;; Also be smart in the minibuffer.
          (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
    (sp-local-pair 'org-mode "~" "~")
    (sp-local-pair 'org-mode "=" "=")
    (sp-local-pair 'org-mode "_" "_")
    (sp-local-pair 'org-mode "/" "/"))
  :demand
  :diminish t
  :ensure t)
