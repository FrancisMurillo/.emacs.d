(require 'prodigy)
(require 'prodigy-set)

(prodigy-set-define-set
 :name "w3m-privacy"
 :strategy 'sequential
 :services '(("w3m-privoxy" . ready)
             ("w3m-polipo" .ready)
             ("w3m-tor" .ready)))

(prodigy-set-define-set
 :name "tor-all"
 :strategy 'parallel
 :services '(("w3m-tor")
             ("erc-tor")))

(prodigy-set-mode t)

(advice-add 'prodigy-start-service :after (lambda (&rest _args) (message "Executing `prodigy-start-service'")))
(advice-add 'prodigy-stop-service :after (lambda (&rest _args) (message "Executing `prodigy-stop-service'")))
(advice-add 'prodigy-restart-service :after (lambda (&rest _args) (message "Executing `prodigy-restart-service'")))

(split-window)
(prodigy)
