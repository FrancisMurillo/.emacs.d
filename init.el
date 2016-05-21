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
    ("0f63b94e366a6a9cd3ac12b3f5e7b88ba214fd592a99fb5bc55af33fb2280c7f" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "a317b11ec40485bf2d2046d2936946e38a5a7440f051f3fcc4cdda27bde6c5d4" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "47ac4658d9e085ace37e7d967ea1c7d5f3dfeb2f720e5dec420034118ba84e17" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" default)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:foreground "#657b83"))))
 '(window-numbering-face ((t (:foreground "tomato" :weight extra-bold))) t))
