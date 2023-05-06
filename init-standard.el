;; The actual init file for my configuration

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(unless (>= emacs-major-version 24)
  (message "This config works only for Emacs version 24 and higher")
  (kill-emacs))

(require 'package)

(defconst fn/cache-dir-name ".cache"
  "Place every moving file in this directory")

(defconst fn/cache-dir (expand-file-name fn/cache-dir-name user-emacs-directory)
  "Every cached or moving file should be here like with Spacemacs")

(make-directory fn/cache-dir t)

(when (version<= "25" emacs-version)
  (require 'nsm)
  (setq nsm-settings-file
        (expand-file-name "network-security" fn/cache-dir)
        network-security-level 'high))

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" fn/cache-dir)))

(setq load-prefer-newer t)

(defconst fn/library-dir (expand-file-name "lib" user-emacs-directory)
  "A library directory for the dependencies.")

(defconst fn/package-dir (expand-file-name "packages" fn/library-dir)
  "A library for my Emacs packages.")

(setq package-user-dir fn/package-dir
      package-gnupghome-dir (expand-file-name "gnupg" fn/package-dir)
      package-enable-at-startup nil)

(package-initialize nil)

(defconst fn/bootstrap-dir (expand-file-name "bootstrap" user-emacs-directory)
  "Bootstrap directory.")

(defconst fn/bootstrap-packages
  `((bind-key . "bind-key.tar")
    (edit-indirect . "edit-indirect.tar")
    (diminish . "diminish.tar")
    (use-package . "use-package.tar")
    (org . "org.tar"))
  "Required bootstrap packages.")

(progn ;; Use Package & Dependencies
  (unless (package-installed-p 'bind-key)
    (package-install-file
     (expand-file-name (cdr (assoc 'bind-key fn/bootstrap-packages))
                       fn/bootstrap-dir)))

  (unless (package-installed-p 'diminish)
    (package-install-file
     (expand-file-name (cdr (assoc 'diminish fn/bootstrap-packages))
                       fn/bootstrap-dir)))

  (unless (package-installed-p 'use-package)
    (package-install-file
     (expand-file-name (cdr (assoc 'use-package fn/bootstrap-packages))
                       fn/bootstrap-dir))))

(unless (package-installed-p 'org) ;; org-mode
  (package-install-file
   (expand-file-name (cdr (assoc 'org fn/bootstrap-packages))
                     fn/bootstrap-dir)))

(defcustom fn/config-file (expand-file-name "config.org" user-emacs-directory)
  "Main org file to load")

(defconst fn/config-backup-file (expand-file-name "working-config.org" fn/bootstrap-dir)
  "Secondary org file in case of error.")


(defun fn/backup-config-file ()
  "Backup main config file for ease of recovery."
  (interactive)
  (copy-file fn/config-file
             fn/config-backup-file
             t)
  (message "Config backup good"))

(defun fn/load-backup-config ()
  "Load secondary config."
  (interactive)
  (org-babel-load-file fn/config-backup-file))

(org-babel-load-file fn/config-file)

(add-hook 'after-init-hook #'fn/backup-config-file)

(defconst fn/post-config-file (expand-file-name ".init-extension.el" "~")
  "Post script to load after the main configuration loads.")

(load fn/post-config-file t)
