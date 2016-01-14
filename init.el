(unless (>= emacs-major-version 24)
  (message "This config works only for Emacs version 24 and higher")
  (kill-emacs))

;; Helper functions
(defun string/ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
	 (let ((elength (length ending)))
	   (string= (substring s (- 0 elength)) ending)))
            (t nil)))

(require 'cl)
(setq load-path
      (remove-if
       (lambda (text) (string/ends-with text "org"))
       load-path))

;; customize loading the packages
(package-initialize t)

;; Modify the hard dependencies
;; org-mode used to be here

;; Load the rest of the packages
(package-initialize nil)

;; Configuration bootstrapping
;; use-package is fundamental to this configuration
(unless (package-installed-p 'use-package)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package)

  (use-package org
    :ensure t)
  (use-package org-plus-contrib
    :ensure t)

  (kill-emacs))

;; This part assumes ony org-babel-load-file is available
(setq package-enable-at-startup nil)
(org-babel-load-file
  (expand-file-name "config.org" user-emacs-directory))
