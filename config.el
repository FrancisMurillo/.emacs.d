(defun fn/reload-config ()
(interactive)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory)))

(setq org-src-tab-acts-natively t)

(setq
 user-full-name "Francis Murillo"
 user-mail-address "francisavmurillo@gmail.com")

(load "secret" t)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")))

(require 'use-package)
(setq use-package-verbose t)

(use-package auto-compile
    :ensure t
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    (setq load-prefer-newer t))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(set-language-environment "UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-conrol t)
(setq backup-by-copying t)
(setq vc-make-backup-files t)
;; FIXME: Hard coded path?
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(savehist-mode 1)

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
  '(kill-ring
    search-ring
    regexp-search-ring))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-whitespace-mode t)
(global-auto-revert-mode t)

(set-frame-font "DejaVu Sans Mono-6" t t)

(load-theme 'tsdh-light)

(global-set-key (kbd "RET") 'newline-and-indent)

(use-package winner
  :ensure t
  :config
  (winner-mode t))

(use-package ido
  :disabled t
  :defer t
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching 1)
  (setq ido-show-dot-for-dired 1)

  ;; vertical ido display is better, like my taskbar
  (use-package ido-vertical-mode
        :ensure t
        :defer t
        :config
        (ido-vertical-mode t)
        (setq ido-vertical-show-count t))

  ;; flex matching is a must
  (use-package flx-ido
        :ensure t
        :defer t
        :config
        (flx-ido-mode t)
        (setq ido-enable-flex-matching t)
        (setq ido-use-faces nil))

  ;; smex is a great addition as well
  (use-package smex
    :ensure t
    :defer t
    :bind (("M-x" . smex)
       ("C-c C-c M-x" . execute-extended-command))
    :config
    (smex-initialize))
  )

(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode t)
)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t))

(add-to-list 'org-modules 'org-drill)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-indexing-method 'native)
  )

(use-package async
:ensure t)

          (use-package helm
:ensure t
:bind (
           ("M-x" . helm-M-x))
:config
(require 'helm-config)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(helm-mode t))

(use-package helm-projectile
:ensure t
:config
(setq projectile-completion-system 'helm)
(helm-projectile-on))

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (setq popup-use-optimized-column-computation nil))

(use-package yasnippet
:ensure t
:defer t)

(use-package emmet-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t)

(use-package js3-mode
  :ensure t
  :defer t)

(defun fn/load-projectile-hook ()
  (interactive)
  (mapcar (lambda (project)
   (setq fn/current-project (expand-file-name project))
   (load
    (expand-file-name ".projectile-hook" fn/current-project)
    t))
    projectile-known-projects))
