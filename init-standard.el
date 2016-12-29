;; The actual init file for my configuration


;; MVC Emacs
(require 'server)
(unless (eq (server-running-p) t)
  (server-start))


;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(unless (>= emacs-major-version 24)
  (message "This config works only for Emacs version 24 and higher")
  (kill-emacs))


;; Package Manager
(require 'package)

(setq load-prefer-newer t)


(defconst fn/library-dir (expand-file-name "lib" user-emacs-directory)
  "A library directory for the dependencies.")

(defconst fn/package-dir (expand-file-name "packages" fn/library-dir)
  "A library for my Emacs packages.")

(setq package-user-dir fn/package-dir
   package-enable-at-startup nil)

(package-initialize nil)


;; Bootstrapping
(defconst fn/bootstrap-dir (expand-file-name "bootstrap" user-emacs-directory)
  "Bootstrap directory.")

(defconst fn/bootstrap-packages (list 'use-package 'org 'org-plus-contrib)
  "Required bootstrap packages.")

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (let ((package-file
          (expand-file-name
           (format "%s.tar" (symbol-name package))
           fn/bootstrap-dir)))
       (package-install-file package-file))))
 fn/bootstrap-packages)


;; Preconfig
(defcustom fn/pre-config-file "pre-config.el"
  "File script to load before the main configuration loads, useful for setting options")

(load (expand-file-name fn/pre-config-file user-emacs-directory) t)


;; Block Tagging
(defcustom fn/config-file (expand-file-name "config.org" user-emacs-directory)
  "Main org file to load")


(defconst fn/code-block-id-symbol :block-id
  "My default tangler block id.")

(defconst fn/code-block-start-format ";; --- begin block: %s ---"
  "The code block start format.")

(defconst fn/code-block-end-format   ";; --- end block:   %s ---"
  "The code block end format.")


(defvar fn/current-org-block-info nil
  "The current block info being tangled by `org-babel-tangle-single-block'.
Hacked on v9 since it is lexically binded.")

(defun fn/set-current-org-block-info (orig-tangle &rest args)
  "Set `fn/current-org-block-info' with the current code block being tangled."
  (prog2
      (setq fn/current-org-block-info (org-babel-get-src-block-info))
      (apply orig-tangle args)
    (setq fn/current-org-block-info nil)))

(advice-add 'org-babel-tangle-single-block :around #'fn/set-current-org-block-info)

(defun fn/org-babel-tangle-wrap-block-info ()
  "Wraps a code block with `fn/code-block-id-symbol'."
  (let* ((block-params (nth 2 fn/current-org-block-info))  ;; org-babel-tangle binding
      (block-id (cdr (assoc fn/code-block-id-symbol block-params))))
    (when block-id
      (let ((block-start (format fn/code-block-start-format block-id))
          (block-end (format fn/code-block-end-format block-id)))
        (save-excursion
          (beginning-of-buffer)
          (insert block-start)
          (insert "\n")

          (end-of-buffer)
          (insert "\n")
          (insert block-end))))))

(add-hook 'org-babel-tangle-body-hook #'fn/org-babel-tangle-wrap-block-info)


;; Config & Backup
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


;; Post Config
(defconst fn/post-config-file (expand-file-name ".init-extension.el")
  "Post script to load after the main configuration loads.")

(load (expand-file-name fn/post-config-file "~") t)
