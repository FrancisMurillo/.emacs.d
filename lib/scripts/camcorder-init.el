(require 'org)
(require 'org-jekyll-blogger)

(when (file-exists-p "~/Fakespace/jekyll-project")
  (delete-directory "~/Fakespace/jekyll-project" t t))

(when (file-exists-p "~/Fakespace/jekyll-org-project")
  (delete-directory "~/Fakespace/jekyll-org-project" t t))

(setq project (org-jekyll-blogger-define-project
            "my-project"
            :project-root "~/Fakespace/jekyll-org-project"
            :publish-root "~/Fakespace/jekyll-project"))

(setq main-blog (org-jekyll-blogger-define-blog
              "main"
              :project project
              :post-options
              (list
               :category (list "main")))
   second-blog (org-jekyll-blogger-define-blog
                "secondary"
                :project project
                :post-options
                (list
                 :category (list "second"))))
