;; -*- lexical-binding: t; -*-
(pcase (buffer-name)
  ("config.org"
   (fn/use-feature org-src-alert
     (org alert)
     (defun fn/org-src-notify-raw-block-editing (start end length)
       "Notify me when I am editing an raw org-src block given START, END and LENGTH"
       (when (org-in-src-block-p t)
         (run-at-time 0 nil
                      (lambda ()
                        (org-edit-special)

                        (alert "[config.org] Avoid modifying a org-src-block directly. Use `org-edit-src-code'."
                               :category 'config
                               :style 'fringe
                               :severity 'high)))))

     (add-to-list 'after-change-functions #'fn/org-src-notify-raw-block-editing))

   (local-set-key (kbd "C-c b C-c")  #'fn/update-code-block)
   (local-set-key (kbd "C-c b C-b") #'fn/generate-code-block-id)

   ;; These pair in the following order
   (add-hook 'fn/org-edit-src-exit-hook #'fn/generate-code-block-id t t)))
