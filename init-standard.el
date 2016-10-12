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

(defcustom fn/config-file (expand-file-name "config.org" user-emacs-directory)
  "Main org file to load")




(defconst fn/code-block-id-symbol :block-id
  "My default tangler block id.")

(defconst fn/code-block-start-format ";; --- begin block: %s ---"
  "The code block start format.")

(defconst fn/code-block-end-format   ";; --- end block:   %s ---"
  "The code block end format.")


(defun fn/org-babel-tangle-wrap-block-info ()
  "Wraps a code block with `fn/code-block-id-symbol'.
If you want to use directly, set `info' to `(org-babel-tangle-get-src-block-info'."
  (let* ((block-params (nth 2 info))  ;; org-babel-tangle binding
         (block-id (cdr (assoc fn/code-block-id-symbol params))))
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

(defmacro fn/code-block-safety (block-id &rest body)
  "Wraps a buffer with a block safety given BLOCK-ID and BODY."
  (let ((error-symbol (make-symbol "ex")))
    `(condition-case ,error-symbol
         ,@body
       ('error
        (message "Error loading block %s: %s"
                 ,block-id
                 (error-message-string ,error-symbol))))))

(defun fn/org-babel-tangle-wrap-block-safety ()
  "Wraps a code block with `fn/code-block-'"
  (let* ((block-params (nth 2 info))  ;; org-babel-tangle binding
         (block-id (cdr (assoc fn/code-block-id-symbol params))))
    (when block-id
      (let ((block-start (format fn/code-block-start-format block-id))
            (block-end (format fn/code-block-end-format block-id)))
        (save-excursion
          (beginning-of-buffer)
          (insert (format "(fn/code-block-safety \"%s\" " block-id))
          (insert "\n")

          (end-of-buffer)
          (insert "\n")
          (insert ")"))))))


;; (add-hook 'org-babel-tangle-body-hook #'fn/org-babel-tangle-wrap-block-safety)
(add-hook 'org-babel-tangle-body-hook #'fn/org-babel-tangle-wrap-block-info)


(defconst fn/config-backup-file (expand-file-name ".safe-config.org" user-emacs-directory)
  "Secondary org file in case of error.")


(defun fn/backup-config-file ()
  "Backup main config file for ease of recovery."
  (interactive)
  (copy-file fn/config-file
             fn/config-backup-file
             t)
  (message "Config backup good"))

(defun fn/load-backup-config ()
  "Load secondary config"
  (interactive)
  (org-babel-load-file fn/config-backup-file))


(org-babel-load-file fn/config-file)


(add-hook 'after-init-hook #'fn/backup-config-file)


;; Environement specific files, don't commit that file
(defcustom fn/post-config-file ".init-extension.el"
  "Post script to load after the main configuration loads")

(load (expand-file-name fn/post-config-file "~") t)
