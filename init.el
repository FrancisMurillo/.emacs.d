;; This customized init file allows to load a default home or a custom home to

(setq fn/home-switch "-H")

(setq fn/standard-init-file "init-standard.el"
      fn/default-init-file "init.el")


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
      (setq user-emacs-directory new-home
            user-init-file (expand-file-name fn/default-init-file user-emacs-directory)
            bootstrap-file user-init-file))
    (load bootstrap-file))

  ;; This is to let the home switch pass through normally, not as a invalid argument
  (add-to-list 'command-switch-alist '(fn/home-switch . identity)))

(defun fn/bootstrap-new-emacs (new-home)
  (async-shell-command (mapconcat 'identity
                                  (list "emacs"
                                        "--debug-init"
                                        fn/home-switch
                                        new-home)
                                  " "))
  (message (mapconcat 'identity (list
                                 "Bootstrapped Emacs from"
                                 new-home
                                 "so go open that window since I can't... for now")
                      " ")))

(fn/bootstrap-emacs)
