;; This customized init file allows to load a default home or a custom
;; home to


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize) ;; This is handled by `init-standard.el` instead.

(defconst fn/home-switch "-H"
  "Command arg to indicate if it using a custom home directory.")

(defconst fn/standard-init-file-name "init-standard.el"
  "My init file name.")

(defconst fn/default-init-file-name "init.el"
  "The default init file name.")


(defun fn/get-switch-arg (switch)
  "Get the `fn/home-switch' value"
  (let ((found-switch (member switch command-line-args)))
    (if found-switch
        (car (cdr found-switch))
      nil)))

(defun fn/bootstrap-emacs ()
  "Bootstrap Emacs with the correct init file."
  ;; Reference: https://stackoverflow.com/questions/2112256/emacs-custom-command-line-argument
  (let ((new-home (fn/get-switch-arg fn/home-switch))
      (bootstrap-file
       (expand-file-name fn/standard-init-file-name user-emacs-directory)))
    (when new-home
      (setq user-emacs-directory new-home user-init-file
         (expand-file-name fn/default-init-file-name user-emacs-directory)
         bootstrap-file user-init-file))
    (load bootstrap-file))

  ;; This is to let the home switch pass through normally, not as a invalid argument
  (add-to-list 'command-switch-alist (cons fn/home-switch 'identity)))

(defun fn/bootstrap-new-emacs (new-home)
  "Bootstrap new Emacs."
  (async-shell-command
   (string-join
    (list "emacs" "--debug-init"
       fn/home-switch new-home)
    " "))
  (message
   "Bootstrapped Emacs from %s."
   new-home))


(fn/bootstrap-emacs)
