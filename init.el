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

;; customize loading the packages
(package-initialize t)

;; Modify the hard dependencies
;; Removed cedet

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

;; Environement specific files, don't commit that file
(load "~/.init-extension.el" t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c58382b9c4fff1aa94b8e3f0f81b0212bb554e83f76957bab735f960a4c441b1" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "889a93331bc657c0f05a04b8665b78b3c94a12ca76771342cee27d6605abcd0e" "47ac4658d9e085ace37e7d967ea1c7d5f3dfeb2f720e5dec420034118ba84e17" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(window-numbering-face ((t (:foreground "tomato" :weight extra-bold))) t))
