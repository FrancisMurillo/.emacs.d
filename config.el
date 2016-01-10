;; TODO: Is this the correct naming convention?
(defun reload-config ()
(interactive)
(org-babel-load-file
(expand-file-name "config.org" user-emacs-directory)))

(setq
user-full-name "Francis Murillo"
user-mail-address "francisavmurillo@gmail.com")

(load "secret" t)

(unless (assoc-default "melpa" package-archives)

(add-to-list 'package-archives
'("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
'("org" . "http://orgmode.org/elpa/"))   
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
'("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-refresh-contents))

;; Install and configure use-package
(unless 
(package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(setq use-package-verbose t)

;; Install and configure auto-compile
(use-package auto-compile
:ensure t
:config 
(auto-compile-on-load-mode)
(auto-compile-on-save-mode))

(setq load-prefer-newer t)

(setq backup-directory-alist '(("." . (expand-file-name "backups" user-emacs-directory))))

(setq delete-old-versions -1) ;; Keep backups
(setq version-conrol t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" (expand-file-name "auto-save-list") t)))
