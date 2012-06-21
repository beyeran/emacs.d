
;;
;; file: beyeran-jekyll.el
;;

(sys-diversification
 (setq *jekyll-path* "~/projects/beyeran/")
 (setq *jekyll-path* "~/Projects/beyeran/"))

(setq org-publish-project-alist
      '(("beyeran-website-jekyll"
        ;; Path to org files
        :base-directory (concat *jekyll-path* "org/")
        :base-extension "org"

        ;; Path to Jekyll project.
        :publishing-directory (concat *jekyll-path* "jekyll/")
        :recursive t
        :publishing-function org-publish-org-to-html
        :headline-levels 4
        :html-extension "html"
        :body-only t ;; Only export section between <body> </body>
        )

        ("beyeran-website-static"
         :base-directory (concat *jekyll-path* "org/")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :publishing-directory "~/Projects/beyeran-jekyll/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("beyeran-website" :components ("beyeran-website-jekyll" "beyeran-website-static"))))

(provide 'beyeran-jekyll)
