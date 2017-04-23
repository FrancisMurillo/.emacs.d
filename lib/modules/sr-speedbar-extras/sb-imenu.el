;;; sb-imenu.el  -*- lexical-binding: t; -*-

(require 'imenu)
(require 'speedbar)

(defvar sb-imenu-key-map nil
  "speedbar imenu keymap.")

(defun sb-imenu-install-speedbar-variables ()
  "Install speedbar variables."
  (setq sb-imenu-key-map (speedbar-make-specialized-keymap))
  (define-key sb-imenu-key-map (kbd "RET") 'speedbar-edit-line))

(if (featurep 'speedbar)
    (sb-imenu-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'sb-imenu-install-speedbar-variables))

(speedbar-add-expansion-list '("sb-imenu" nil sb-imenu-key-map sb-imenu-buttons))

(defvar sb-imenu-active-buffer nil)

(defun sb-imenu-buttons (dir depth)
  "Show imenu tags for current buffer."
  (let (tags buf buf-name)
    (sb-imenu-get-active-buffer)
    (when sb-imenu-active-buffer
      (with-current-buffer sb-imenu-active-buffer
        (setq buf-name (or (buffer-file-name) (buffer-name)))
        (setq imenu--index-alist nil)
        (condition-case nil
            (imenu--make-index-alist t)
          (error nil))
        (setq tags (copy-alist imenu--index-alist)))
      (insert buf-name ":\n\n")
      (when tags
        (when (string= (caar tags) "*Rescan*")
          (setq tags (cdr tags)))
        (when tags
          (sb-imenu-populate tags 0))))))

(defun sb-imenu-get-active-buffer ()
  "Get the active buffer."
  (setq sb-imenu-active-buffer nil)
  (condition-case nil
      (with-selected-frame (dframe-select-attached-frame (speedbar-current-frame))
        (sb-imenu-get-interesting-buffer))
    (error nil))
  (unless sb-imenu-active-buffer
    (sb-imenu-get-interesting-buffer)))

(defun sb-imenu-get-interesting-buffer ()
  "Get an interesting buffer."
  (catch 'done
    (dolist (buffer (buffer-list))
      (unless (string-match "^[ *]" (buffer-name buffer))
        (setq sb-imenu-active-buffer buffer)
        (throw 'done buffer)))))

(defun sb-imenu-populate (tags level)
  "Populate speedbar from imenu tags."
  (dolist (item tags)
    (if (imenu--subalist-p item)
        (progn
          (speedbar-make-tag-line 'curly
                                  ?- 'sb-imenu-expand-line
                                  (cdr item)
                                  (car item) 'sb-imenu-expand-line (cdr item)
                                  'font-lock-type-face level)
          (sb-imenu-populate (cdr item) (1+ level)))
      (speedbar-make-tag-line 'statictag
                              nil nil
                              nil
                              (car item) 'sb-imenu-go (cdr item)
                              'font-lock-variable-name-face level))))

(defun sb-imenu-expand-line (text token indent)
  "Expand/contract the item under the cursor."
  (interactive)
  (if (save-excursion (beginning-of-line)
                      (looking-at "[0-9]+:\\s-*{[+]}"))
      ;; Expand
      (progn
        (speedbar-change-expand-button-char ?-)
        (forward-line)
        (speedbar-with-writable
          (save-excursion
            (sb-imenu-populate token (1+ indent)))))
    ;; Contract
    (speedbar-change-expand-button-char ?+)
    (speedbar-with-writable
      (save-excursion
        (forward-line)
        (while (and (not (eobp))
                    (looking-at "\\([0-9]+\\):")
                    (> (string-to-number (match-string-no-properties 1)) indent))
          (delete-region (point-at-bol) (1+ (point-at-eol))))))))

(defun sb-imenu-go (text node indent)
  "Goto the current tag."
  (interactive)
  (condition-case nil
      (progn
        (speedbar-select-attached-frame)
        (raise-frame)
        (select-frame-set-input-focus (selected-frame)))
    (error nil))
 (switch-to-buffer sb-imenu-active-buffer)
  (goto-char node))

(defun sb-imenu-refresh ()
  "Refresh the speedbar."
  (let ((buf sb-imenu-active-buffer))
    (sb-imenu-get-active-buffer)
    (unless (equal buf sb-imenu-active-buffer)
      (speedbar-refresh))))

(add-hook 'speedbar-timer-hook 'sb-imenu-refresh)

(provide 'sb-imenu)
