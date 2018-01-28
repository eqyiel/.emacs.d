;;; eqyiel-org.el

(require 'eqyiel-ox-rkm-html)

(eval-after-load "eqyiel-ox-rkm-html"
  '(let ((rkm.id.au-base-directory "~/doc/rkm.id.au/src/")
         (rkm.id.au-publishing-directory "~/doc/rkm.id.au/pub/")
         (rkm.id.au-link-home "https://rkm.id.au/")
         (rkm.id.au-link-rss "https://rkm.id.au/rss/")
         (rkm.id.au-title "rkm.id.au")
         (rkm.id.au-description
          "Tech recipes and talking smack about free software.")
         (rkm.id.au-lang "en")
         (rkm.id.au-email user-mail-address)
         (rkm.id.au-author user-full-name)
         (rkm.id.au-html-head-template
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/head.html"))
         (rkm.id.au-preamble-template
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/preamble.html"))
         (rkm.id.au-analytics-script
          (ox-rkm-html-string-from-file
           "~/doc/rkm.id.au/src/templates/analytics.html"))
         (rkm.id.au-extra-scripts '("/js/bootstrap.min.js")))
     (setq org-publish-project-alist
           `(("rkm.id.au"
              :components ("rkm.id.au-posts" "rkm.id.au-static" "rkm.id.au-base"
                           "rkm.id.au-css" "rkm.id.au-js" "rkm.id.au-favicon"
                           "rkm.id.au-robots"))
             ("rkm.id.au-posts"
              :base-directory
              ,(concat (file-name-directory rkm.id.au-base-directory) "posts/")
              :base-extension "org"
              :completion-function ox-rkm-html-posts-completion-function
              :description ,rkm.id.au-description
              :html-head ,rkm.id.au-html-head-template
              :html-link-home ,rkm.id.au-link-home
              :html-postamble ox-rkm-html-postamble
              :html-preamble ,rkm.id.au-preamble-template
              :preparation-function ox-rkm-html-posts-preparation-function
              :publishing-directory ,rkm.id.au-publishing-directory
              :publishing-function ox-rkm-html-publish-to-rkm-html
              :recursive t
              :rkm-html-extra-scripts ,rkm.id.au-extra-scripts
              :rkm-html-rss-email ,rkm.id.au-email
              :rkm-html-rss-link ,rkm.id.au-link-rss
              :rkm-html-rss-title ,rkm.id.au-title
              :rkm-html-analytics-script ,rkm.id.au-analytics-script)
             ("rkm.id.au-base"
              :base-directory
              ,(concat (file-name-directory rkm.id.au-base-directory) "base/")
              :base-extension "org"
              :description ,rkm.id.au-description
              :html-head ,rkm.id.au-html-head-template
              :html-link-home ,rkm.id.au-link-home
              :html-postamble nil
              :html-preamble ,rkm.id.au-preamble-template
              :publishing-directory ,rkm.id.au-publishing-directory
              :publishing-function ox-rkm-html-publish-base
              :recursive t
              :rkm-html-extra-scripts ,rkm.id.au-extra-scripts
              :rkm-html-analytics-script ,rkm.id.au-analytics-script)
             ("rkm.id.au-static"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "static/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "static/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-css"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "css/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "css/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-js"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "js/")
              :base-extension any
              :publishing-directory
              ,(concat
                (file-name-directory rkm.id.au-publishing-directory)
                "js/")
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-favicon"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "favicon/dist/")
              :base-extension any
              :publishing-directory
              ,(file-name-directory rkm.id.au-publishing-directory)
              :publishing-function org-publish-attachment
              :recursive t)
             ("rkm.id.au-robots"
              :base-directory
              ,(concat
                (file-name-directory rkm.id.au-base-directory)
                "robots/")
              :base-extension any
              :publishing-directory
              ,(file-name-directory rkm.id.au-publishing-directory)
              :publishing-function org-publish-attachment
              :recursive t)))))

(provide 'eqyiel-org)
