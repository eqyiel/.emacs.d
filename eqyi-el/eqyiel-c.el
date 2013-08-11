;;; eqyiel-c.el

;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(setq semanticdb-default-save-directory "~/.cache/emacs/semanticdb")

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-mru-bookmark-mode
        global-semantic-idle-local-symbol-highlight-mode))

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (global-ede-mode t)

;; For correct work of Semantic with С & C++ code it's recommended to use the
;; EDE package (it allows to work with projects, etc.).  For these languages,
;; EDE package defines special project type: ede-cpp-root-project, that provides
;; additional information to Semantic, and this information will be used to
;; analyze source code of your project.

;; (ede-cpp-root-project "Test"
;;                       :name "Test Project"
;;                       :file "~/work/project/CMakeLists.txt"
;;                       :include-path '("/"
;;                                       "/Common"
;;                                       "/Interfaces"
;;                                       "/Libs"
;;                                       )
;;                       :system-include-path '("~/exp/include")
;;                       :spp-table '(("isUnix" . "")
;;                                    ("BOOST_TEST_DYN_LINK" . "")))

(defun ac-c-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet
                             ac-source-filename
                             ac-source-gtags
                             ac-source-semantic))))

(add-hook 'c-mode-common-hook 'ac-c-mode-setup)

(semantic-mode 1)

(provide 'eqyiel-c)
