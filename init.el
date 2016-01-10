; This sets up the load path so that we can override it
(package-initialize nil)

;; Override the packages with the git version of Org and other packages

;; Static org-mode
(add-to-list 'load-path 
	     (expand-file-name "elisp/org-mode/lisp" user-emacs-directory)) 
(add-to-list 'load-path 
	     (expand-file-name "elisp/org-mode/contrib/lisp" user-emacs-directory))

;; Load the rest of the packages
(package-initialize nil)

;; FIXME: Can loading org not be hard coded?
(load-library "org")


(setq package-enable-at-startup nil)
(org-babel-load-file 
 (expand-file-name "config.org" user-emacs-directory))
