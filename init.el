; This sets up the load path so that we can override it
(package-initialize nil)

;; Load the rest of the packages
(package-initialize nil)

;; If use-package is not installed, assume first run
;; Install the use-package and org
(unless (package-installed-p 'use-package)
  (package-install "use-package")
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (require 'use-package)

  (use-package org
    :ensure t
    :pin org)
  (use-package org-plus-contrib
    :ensure t
    :pin org)
  (kill-emacs))



(setq package-enable-at-startup nil)
(org-babel-load-file 
 (expand-file-name "config.org" user-emacs-directory))
