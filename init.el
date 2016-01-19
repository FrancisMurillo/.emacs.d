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
;; cedet
(let ((default-directory
        (expand-file-name "elisp/cedet/lisp/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Fakespace/nobody-library/todo.org" "/home/fnmurillo/Fakespace/nobody-library/learning.org" "/home/fnmurillo/Fakespace/nobody-library/diary/2016-01-18.journal.org.gpg" "/home/fnmurillo/Fakespace/nobody-library/capture.org" "/home/fnmurillo/Fakespace/nobody-library/main.org" "/home/fnmurillo/Fakespace/nobody-library/fnlog.org")))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "git commit -a -m \"Update\" && git push origin master")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
