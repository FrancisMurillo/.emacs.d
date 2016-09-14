;; This customized init file allows to load a default home or a custom
;; home to


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize) ;; This is handled by `init-standard.el` instead.

(setq fn/home-switch "-H")

(setq fn/standard-init-file "init-standard.el" fn/default-init-file "init.el")


(defun fn/get-switch-arg (switch)
  (let ((found-switch (member switch command-line-args)))
    (if found-switch
        (car (cdr found-switch))
      nil)))

(defun fn/bootstrap-emacs ()
;;; Reference: https://stackoverflow.com/questions/2112256/emacs-custom-command-line-argument
  (let ((new-home (fn/get-switch-arg fn/home-switch))
        (bootstrap-file (expand-file-name fn/standard-init-file user-emacs-directory)))
    (when new-home
      (setq user-emacs-directory new-home user-init-file (expand-file-name fn/default-init-file
                                                                           user-emacs-directory)
            bootstrap-file user-init-file))
    (load bootstrap-file))

  ;; This is to let the home switch pass through normally, not as a invalid argument
  (add-to-list 'command-switch-alist '(fn/home-switch . identity)))

(defun fn/bootstrap-new-emacs (new-home)
  (async-shell-command (mapconcat 'identity (list "emacs" "--debug-init" fn/home-switch new-home)
                                  " "))
  (message (mapconcat 'identity (list "Bootstrapped Emacs from" new-home
                                      "so go open that window since I can't... for now") " ")))

(fn/bootstrap-emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-status-emoji yahoo-weather all-the-icons helm-smex smex helm-fuzzier helm-flx jekyll-modes rainbow-mode contrast-color colorsarenice-theme ocodo-svg-modelines zone-sl zone-rainbow zone-nyan workgroups2 window-numbering which-key wgrep-helm web-mode web-beautify virtualenvwrapper virtualenv use-package undo-tree twittering-mode tronesque-theme telephone-line spaceline smartparens smart-mode-line-powerline-theme slack skewer-mode shm screenshot sass-mode rvm robe react-snippets rainbow-delimiters py-autopep8 prodigy polymode pcomplete-extension pcmpl-pip pcmpl-git paredit-everywhere paradox pacmacs ox-reveal overseer org-plus-contrib org-journal omnisharp octicons nyan-mode npm-mode nodejs-repl micgoline memory-usage main-line magit-svn lispy ledger-mode latex-preview-pane keyfreq json-snatcher jsfmt js2-refactor js-doc jedi jdee jabber hungry-delete hindent helm-swoop helm-projectile helm-describe-modes helm-descbinds helm-ag header2 haskell-snippets haskell-emacs golden-ratio github-clone git-timemachine git-gutter gist ggtags free-keys flycheck-pos-tip flycheck-mix flycheck-ledger flycheck-haskell flycheck-elm flycheck-elixir flappymacs fireplace fill-column-indicator figlet f3 expand-region eslint-fix enh-ruby-mode engine-mode emr emms emmet-mode emamux emacsshot elpy elm-yasnippets elm-mode elixir-yasnippets elfeed elein elang ein dumb-jump dockerfile-mode docker counsel conkeror-minor-mode company-web company-tern company-jedi company-ghci company-ghc company-auctex command-log-mode column-marker color-theme-approximate color-identifiers-mode cask-mode cask buttercup bbdb base16-theme auto-compile aurel apropospriate-theme alchemist airline-themes ag))))
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
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
