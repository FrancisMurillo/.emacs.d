;; The actual init file for my configuration


;; MVC Emacs
(require 'server)
(server-start)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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


(setq load-prefer-newer t)


(require 'package)

(defconst fn/library-dir (expand-file-name "lib" user-emacs-directory)
  "A library directory for the dependencies.")

(defconst fn/package-dir (expand-file-name "packages" fn/library-dir)
  "A library for my emacs packages.")

(setq package-user-dir fn/package-dir)

;; customize loading the packages
(package-initialize t)

;; Load the rest of the packages
(package-initialize nil)

;; Configuration bootstrapping
;; use-package is fundamental to this configuration
(unless (package-installed-p 'use-package)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

  (package-refresh-contents)
  (package-install 'use-package)


  (use-package org
    :ensure t)
  (use-package org-plus-contrib
    :ensure t)

  (kill-emacs))

(require 'use-package)

(defcustom fn/pre-config-file "pre-config.el"
  "File script to load before the main configuration loads, useful for setting options")

(load (expand-file-name fn/pre-config-file user-emacs-directory) t)

;; This part assumes ony org-babel-load-file is available
(setq package-enable-at-startup nil)

(defcustom fn/config-file "config.org"
  "Main org file to load")

(org-babel-load-file
 (expand-file-name fn/config-file user-emacs-directory))


;; Environement specific files, don't commit that file
(defcustom fn/post-config-file ".init-extension.el"
  "Post script to load after the main configuration loads")

(load (expand-file-name fn/post-config-file "~") t)
