;;; redmine.el --- Emacs interface to Redmine issue tracking system 


(defcustom redmine-url ""
  "Redmine url"
  :type 'string
  :group 'redmine)

(defcustom redmine-login-key ""
  "Redmine login key"
  :type 'string
  :group 'redmine)

(defcustom redmine-project-name ""
  "Redmine project name"
  :type 'string
  :group 'redmine)

(defcustom redmine-program ""
  "Redmine command program (the location of the redmine.py file)"
  :type 'string
  :group 'redmine)

(defconst redmine-sprint-id-regex "^[^:]*:\\([^:]*\\):.*$"
  "Regex for sprint id.")

(defconst redmine-issue-id-regex "^[^:]*:issue\\([^:]*\\):.*$"
  "Regex for issue id.")

(defconst redmine-task-id-regex "^[^:]*:task\\([^:]*\\):.*$"
  "Regex for task id.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constant variables
;; (defconst redmine-issue-id-regex "^[^m]*m\\([^\t]*\\).*$"
;;   "Regex for issue id.")

;;(defconst redmine-issue-id-regex-in-issue "^Issue \\([^ ]*-[^ ]*\\)"
;; (defconst redmine-issue-id-regex-in-issue "^Issue \\(.*\\)"
;;   "Regex for issue when showing it")
;; 
;; (defconst redmine-issue-guid-regex "^[_ ]+[^:]+: \\([^:\n]+\\) :.*$"
;;   "Regex for issue guid.")
;; 
;; (defconst redmine-release-name-regex "^\\(\\)\\(.*\\)$"
;;   "Regex for release id.")
;; 
;; (defconst redmine-feature-name-regex "^:Feature: \\(Version \\)?\\([^\n ]+\\) *.*$"
;;   "Regex for feature id.")
;; 
;; (defconst redmine-issue-description-regex "^.*: (.*) \\(.*\\)$"
;;   "Regex for issue description.")

;; Commands
(defun redmine-show-sprints ()
  "show sprints"
  (interactive)
  (redmine-call-process "sprints" nil "pop"))

(defun redmine-get-sprint-id ()
  (redmine-extract-thing-at-point redmine-sprint-id-regex 1))

(defun redmine-get-issue-id ()
  (or (redmine-extract-thing-at-point redmine-issue-id-regex 1) (redmine-extract-thing-at-point redmine-task-id-regex 1)))

(defun redmine-show-issues-in-sprint ()
  (interactive)
  (let ((sprint-id nil))
    (setq redmine-sprint-id (redmine-get-sprint-id))
    (if redmine-sprint-id
      (redmine-call-process "issues" (concat "--sprint " redmine-sprint-id) "pop")
      (error "Sprint id not found"))))

(defun redmine-show-issues-in-sprint-page2 ()
  (interactive)
  (let ((sprint-id nil))
    (setq redmine-sprint-id (redmine-get-sprint-id))
    (if redmine-sprint-id
      (redmine-call-process "issues" (concat "--sprint " redmine-sprint-id " --page 2") "pop")
      (error "Sprint id not found"))))

(defun redmine-show-issue ()
  (interactive)
  (let ((issue-id nil))
    (setq issue-id (redmine-get-issue-id))
    (if issue-id
	(redmine-call-process "issue" (concat "--issue " issue-id) "pop")
      (error "Issue id not found"))))

(defun redmine-issue-status (new-status)
  (let ((issue-id nil))
    (setq issue-id (redmine-get-issue-id))
    (if issue-id
	(redmine-call-process "set-issue-status" (concat "--issue " issue-id " --status " new-status ) "switch")
      (error "Issue id not found"))))

(defun redmine-issue-status-devdone ()
  (interactive)
  (redmine-issue-status "devdone"))

(defun redmine-issue-status-cantreproduce ()
  (interactive)
  (redmine-issue-status "cantreproduce"))

(defun redmine-issue-status-new ()
  (interactive)
  (redmine-issue-status "new"))

(defun redmine-issue-status-tested ()
  (interactive)
  (redmine-issue-status "tested"))

(defun redmine-issue-status-reopened ()
  (interactive)
  (redmine-issue-status "reopened"))

(defun redmine-add-issue ()
  (interactive)
  (redmine-call-process "new-issue" (concat "--sprint " redmine-sprint-id ) "switch"))

(defun redmine-edit-issue ()
  (interactive)
  (let ((issue-id nil))
    (setq issue-id (redmine-get-issue-id))
    (if issue-id
	(redmine-call-process "edit-issue" (concat "--issue " issue-id) "pop")
      (error "Issue id not found"))))

(defun redmine-delete-issue ()
  (interactive)
  (let ((issue-id nil))
    (setq issue-id (redmine-get-issue-id))
    (if issue-id
	(redmine-call-process "delete-issue" (concat "--issue " issue-id) "pop")
      (error "Issue id not found"))))

(defun redmine-add-issue-journal ()
  (interactive)
  (let ((issue-id nil))
    (setq issue-id (redmine-get-issue-id))
    (if issue-id
	(redmine-call-process "add-issue-journal" (concat "--issue " issue-id) "pop")
      (error "Issue id not found"))))

(defun redmine-everything ()
  (interactive)
  (redmine-call-process "everything" nil "pop"))

(defun redmine-everything-sprint ()
  (interactive)
  (let ((sprint-id nil))
    (setq redmine-sprint-id (redmine-get-sprint-id))
    (if redmine-sprint-id
	(redmine-call-process "everything-sprint" (concat "--sprint " redmine-sprint-id) "pop")
      (error "Sprint id not found"))))

(defun redmine-timesheet-sprint ()
  (interactive)
  (let ((sprint-id nil))
    (setq redmine-sprint-id (redmine-get-sprint-id))
    (if redmine-sprint-id
	(redmine-call-process "time-sheet" (concat "--sprint " redmine-sprint-id) "pop")
      (error "Sprint id not found"))))

;; (defun redmine-show-release ()
;;   "show issues for release."
;;   (interactive)
;; 
;;   (setq release-name (redmine-extract-thing-at-point redmine-release-name-regex 2))
;;     (if release-name
;;         (redmine-call-process "release-summary" (concat "-r" release-name) "switch")
;;       (error "release name not found")))
;; 
;; (defun redmine-todo ()
;;   "Show current todo."
;;   (interactive)
;;   (server-start)
;;   (redmine-call-process "list-releases" nil "pop"))
;; 
;; (defun redmine-todo-org ()
;;   "Show current todo in org-mode compatible format"
;;   (interactive)
;;   (redmine-call-process "time-release-org" nil "pop"))
;; 
;; (defun redmine-todo-org_no_time ()
;;   "Show current todo in org-mode compatible format with no time or status information"
;;   (interactive)
;;   (redmine-call-process "time-release-org-no-time" nil "pop"))
;; 
;; (defun redmine-log ()
;;   "Show log of recent activities."
;;   (interactive)
;;   (redmine-call-process "log" nil "pop"))
;; 
;; (defun redmine-order-up ()
;;   "Move issue up in the release"
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "order-up" issue-id)
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-order-down ()
;;   "Move issue down in the release"
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "order-down" issue-id)
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-edit ()
;;   "edit the issue in plain text"
;;   (interactive)
;;   (let ((issue-guid nil))
;;     (setq issue-guid (redmine-get-issue-guid 1))
;;     (if issue-guid
;; 	(find-file-other-window (concat redmine-last-visited-issue-directory "/.issues/issue-" issue-guid ".json"))
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-master-name ()
;;   "set master names"
;;   (interactive)
;;   (redmine-call-process "number-issues"))
;; 
;; (defun redmine-show ()
;;   "Show issue detale."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "show-issue" (concat "-n " issue-id) "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-remove ()
;;   "Delete an issue"
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "remove" (concat "-n " issue-id) "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-assign ()
;;   "Assign issue to a release."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "assign-release" (concat "-i" issue-id) "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-owner ()
;;   "Set the owner of an issue"
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "owner" (concat "-i" issue-id) "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-assignfeature ()
;;   "Assign issue to a feature."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "assignfeature" issue-id "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-set-feature-time ()
;;   "Set estimate for how long a feature will take."
;;   (interactive)
;;   (let ((feature-name nil))
;;     (setq feature-name (redmine-extract-thing-at-point redmine-feature-name-regex 2))
;;     (if feature-name
;;         (redmine-call-process "set-feature-time" feature-name "switch")
;;       (error "Feature name not found"))))
;; 
;; (defun redmine-unassign ()
;;   "Assign issue to a release."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "unassign" issue-id "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-comment ()
;;   "Edit issue comment."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "comment" issue-id "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-close ()
;;   "Close a issue."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "close" (concat "-n " issue-id) "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-claim ()
;;   "Claim a issue."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (redmine-call-process "claim" issue-id "switch")
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-drop ()
;;   "Drop an issue."
;;   (interactive)
;;   (let ((issue-id nil))
;;     (setq issue-id (redmine-get-issue-id 1))
;;     (if issue-id
;;         (when (yes-or-no-p (concat "Drop " issue-id " "))
;;           (redmine-call-process "drop" issue-id "switch"))
;;       (error "Issue id not found"))))
;; 
;; (defun redmine-release ()
;;   "Mark issues as released."
;;   (interactive)
;;   (let ((release-name nil))
;;     (setq release-name (redmine-extract-thing-at-point redmine-release-name-regex 2))
;;     (if release-name
;;         (redmine-call-process "release" release-name "switch")
;;       (error "Release name not found"))))
;; 
;; (defun redmine-release-feature ()
;;   "Mark issues as released."
;;   (interactive)
;;   (let ((feature-name nil))
;;     (setq feature-name (redmine-extract-thing-at-point redmine-feature-name-regex 2))
;;     (if feature-name
;;         (redmine-call-process "releasefeature" feature-name "switch")
;;       (error "Feature name not found"))))
;; 
;; (defun redmine-get-issue-id (n)
;;   "works in the todo view or in the issue view"
;;   (redmine-extract-thing-at-point redmine-issue-id-regex 1))
;; 
;; (defun redmine-get-issue-guid (n)
;;   "works in the todo view or in the issue view"
;;   (let ((issue-id (redmine-extract-thing-at-point redmine-issue-id-regex 1)))
;;     (shell-command-to-string (redmine-build-command "get-guid" (concat "-n" issue-id)))))
;; 

(defun redmine-extract-thing-at-point (regex n)
  (save-excursion
    (let ((line (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                  (progn (end-of-line) (point)))))
      (when (string-match regex line)
        (match-string n line)))))

(defun redmine-reload ()
  (interactive)
  (let ((prev_line_number (line-number-at-pos)))
    (cond ((string= (buffer-name) "*redmine-issues*")
	   (redmine-call-process "issues" (concat "--sprint " redmine-sprint-id) "switch" prev_line_number)
	  ))))

(defun redmine-close-buffer ()
  "Close redmine buffer."
  (interactive)
  (quit-window))

(defun redmine-call-process (command &optional arg popup-flag prev-line-number)
  "Call redmine process asynchronously according with sub-commands."
  (let* ((buffer (get-buffer-create (concat "*redmine-" command "*")))
         (proc (get-buffer-process buffer)))

    (if (and proc (eq (process-status proc) 'run))
        (when (y-or-n-p (format "A %s process is running; kill it?"
                                (process-name proc)))
          (interrupt-process proc)
          (sit-for 1)
          (delete-process proc))

    (with-current-buffer buffer
      (erase-buffer)
      (buffer-disable-undo (current-buffer)))

    (make-comint-in-buffer "redmine-call-process"
                           buffer shell-file-name nil shell-command-switch
                           (redmine-build-command command arg))

    (cond ((or (eq major-mode 'redmine-mode)
               (string= popup-flag "switch"))
           (switch-to-buffer buffer))
          ((string= popup-flag "pop")
           (pop-to-buffer buffer))
          ((string= popup-flag "display")
           (display-buffer buffer))
          (t
           (set-buffer buffer)))

    (setq redmine-prev-line-number prev-line-number)
    (set-process-sentinel
     (get-buffer-process buffer)
     '(lambda (process signal)
        (when (string= signal "finished\n")
          (with-current-buffer (process-buffer process)
            (redmine-mode)
            (goto-char (point-min))
	    (and redmine-prev-line-number (goto-line redmine-prev-line-number)))))))))

(defvar redmine-last-visited-issue-directory nil)

(defun redmine-build-command (command arg)

    (mapconcat 'identity
               (list redmine-program "--userkey" redmine-login-key "--project" redmine-project-name "--url" redmine-url
		     "--action" command arg) " "))

;; Hooks
(defvar redmine-mode-hook nil
  "*Hooks for Taskpaper major mode")

;; Keymap
(defvar redmine-mode-map (make-keymap)
  "*Keymap for Redmine major mode")

;;(define-key redmine-mode-map "e"    'redmine-everything)
(define-key redmine-mode-map "o"    'redmine-everything-sprint)
(define-key redmine-mode-map "t"    'redmine-timesheet-sprint)
(define-key redmine-mode-map "r"    'redmine-show-issues-in-sprint)
(define-key redmine-mode-map "2"    'redmine-show-issues-in-sprint-page2)
(define-key redmine-mode-map "s"    'redmine-show-issue)
;; (define-key redmine-mode-map "E"    'redmine-edit)
;; (define-key redmine-mode-map "s"    'redmine-show)
(define-key redmine-mode-map "A"    'redmine-add-issue)
(define-key redmine-mode-map "e"    'redmine-edit-issue)
(define-key redmine-mode-map "d"    'redmine-delete-issue)
(define-key redmine-mode-map "j"    'redmine-add-issue-journal)
(define-key redmine-mode-map (kbd "C-c n") 'redmine-issue-status-new)
(define-key redmine-mode-map (kbd "C-c d") 'redmine-issue-status-devdone)
(define-key redmine-mode-map (kbd "C-c a") 'redmine-issue-status-cantreproduce)
(define-key redmine-mode-map (kbd "C-c r") 'redmine-issue-status-reopened)
(define-key redmine-mode-map (kbd "C-c t") 'redmine-issue-status-tested)

(define-key redmine-mode-map "g"    'redmine-reload)
;; (define-key redmine-mode-map "d"    'redmine-remove)
;; (define-key redmine-mode-map "t"    'redmine-todo)
;; (define-key redmine-mode-map "o"    'redmine-todo-org)
;; (define-key redmine-mode-map "O"    'redmine-todo-org_no_time)
;; (define-key redmine-mode-map "s"    'redmine-show)
;; (define-key redmine-mode-map "\C-m" 'redmine-show)

;; (define-key redmine-mode-map "a"    'redmine-assign)
;; (define-key redmine-mode-map "o"    'redmine-owner)
;; (define-key redmine-mode-map "w"    'redmine-assignfeature)
;; (define-key redmine-mode-map "U"    'redmine-unassign)
;; (define-key redmine-mode-map "D"    'redmine-drop)
;; (define-key redmine-mode-map "e"    'redmine-edit)
;; (define-key redmine-mode-map "+"    'redmine-comment)

;; (define-key redmine-mode-map "l"    'redmine-claim)
;; (define-key redmine-mode-map "r"    'redmine-release)
;; (define-key redmine-mode-map "Q"    'redmine-add-feature)
;; (define-key redmine-mode-map "\C-Q" 'redmine-release-feature)
(define-key redmine-mode-map "q"    'redmine-close-buffer)
;; (define-key redmine-mode-map "u"    'redmine-order-up)
;; (define-key redmine-mode-map "d"    'redmine-order-down)

;; Face
(defface redmine-release-open-name-face
  '((((class color) (background light))
     (:foreground "coral"))
    (((class color) (background dark))
     (:foreground "coral")))
  "Face definition for open release")

(defface redmine-release-closed-name-face
  '((((class color) (background light))
     (:foreground "seashell4" :slant italic))
    (((class color) (background dark))
     (:foreground "seashell4" :slant italic)))
  "Face definition for closed issues")

(defface redmine-issue-id-face
  '((((class color) (background light))
     (:foreground "slate gray" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "slate gray" :underline t :weight bold)))
  "Face definition for issue id")

(defface redmine-task-id-face
  '((((class color) (background light))
     (:foreground "light gray" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "light gray" :underline t :weight bold)))
  "Face definition for task id")

(defface redmine-release-name-face
  '((((class color) (background light))
     (:foreground "seashell4" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "seashell4" :underline t :weight bold)))
  "Face definition for release name")

(defface redmine-issue-detail-face
  '((((class color) (background light))
     (:foreground "seashell4" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "seashell4" :underline t :weight bold)))
  "Face definition for issue detail")

(defface redmine-issue-closed-name-face
  '((((class color) (background light))
     (:foreground "seashell4" :slant italic))
    (((class color) (background dark))
     (:foreground "seashell4" :slant italic)))
  "Face definition for closed issues")

(defface redmine-issue-text-closed-name-face
  '((((class color) (background light))
     (:foreground "seashell4" :slant italic))
    (((class color) (background dark))
     (:foreground "seashell4" :slant italic)))
  "Face definition for the text of closed issues")

(defface redmine-issue-tested-name-face
  '((((class color) (background light))
     (:foreground "dark green" :slant italic))
    (((class color) (background dark))
     (:foreground "pale green" :slant italic)))
  "Face definition for the text of tested issues")

(defface redmine-issue-open-name-face
  '((((class color) (background light))
     (:foreground "coral"))
    (((class color) (background dark))
     (:foreground "coral")))
  "Face definition for open issues")

(defface redmine-feature-issue-open-name-face
  '((((class color) (background light))
     (:foreground "palegreen"))
    (((class color) (background dark))
     (:foreground "palegreen")))
  "Face definition for open feature issues")

(defface redmine-issue-text-open-name-face
  '((((class color) (background light))
     (:foreground "coral"))
    (((class color) (background dark))
     (:foreground "coral")))
  "Face definition for the text of open issues")

(defvar redmine-issue-id-face 'redmine-issue-id-face)
(defvar redmine-release-open-name-face 'redmine-release-open-name-face)
(defvar redmine-release-closed-name-face 'redmine-release-closed-name-face)
(defvar redmine-task-id-face 'redmine-task-id-face)
(defvar redmine-release-name-face 'redmine-release-name-face)
(defvar redmine-issue-detail 'redmine-issue-detail-face)
(defvar redmine-issue-closed-name-face 'redmine-issue-closed-name-face)
(defvar redmine-issue-open-name-face 'redmine-issue-open-name-face)
(defvar redmine-issue-text-open-name-face 'redmine-issue-text-open-name-face)
(defvar redmine-feature-issue-open-name-face 'redmine-feature-issue-open-name-face)
(defvar redmine-issue-text-closed-name-face 'redmine-issue-text-closed-name-face)
(defvar redmine-issue-tested-name-face 'redmine-issue-tested-name-face)

(defvar redmine-font-lock-keywords
  '(
    ;;("^.*:\\([^:]*\\):.*$" (1 redmine-release-open-name-face t))
    ("\\(.*(New).*\\)" (1 redmine-issue-open-name-face t))
    ("\\(.*(Reopened).*\\)" (1 redmine-issue-open-name-face t))
    ("\\(.*(Dev done).*\\)" (1 redmine-issue-closed-name-face t))
    ("\\(.*(closed).*\\)" (1 redmine-release-closed-name-face t))
    ("\\(.*(Config Required).*\\)" (1 redmine-release-closed-name-face t))
    ("\\(.*(Help Reproduce).*\\)" (1 redmine-release-closed-name-face t))
    ("\\(.*(Duplicate).*\\)" (1 redmine-issue-tested-name-face t))
    ("\\(.*(open).*\\)" (1 redmine-release-open-name-face t))
    ("\\(.*(Tested).*\\)" (1 redmine-issue-tested-name-face t))
    ("^\\(Subject:.*\\)$" (1 redmine-issue-detail t))
    ("\\(Status: New.*\\)" (1 redmine-issue-open-name-face t))
))  

;; Redmine major mode
(define-derived-mode redmine-mode fundamental-mode "Redmine"
  "Major mode Redmine information."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'redmine-mode)
  (setq mode-name "Redmine")
  (use-local-map redmine-mode-map)
  (set (make-local-variable 'font-lock-defaults)  '(redmine-font-lock-keywords))
  (font-lock-mode 1)
  (run-hooks 'redmine-mode-hook))

(provide 'redmine)
;;; redmine.el ends here


;;; Allow to copy links from org mode release buffer
;; (org-add-link-type "redmine" 'org-redmine-open)
;; (add-hook 'org-store-link-functions 'org-redmine-store-link)
;; 
;; (defun org-redmine-open (link)
;;   (when (string-match "\\(.*\\):\\([0-9]+\\)$"  link)
;;     (let* ((path (match-string 1 link))
;;            (page (string-to-number (match-string 2 link))))
;;       (org-open-file path 1) ;; let org-mode open the file (in-emacs = 1)
;;       ;; so that org-link-frame-setup is respected
;;       (doc-view-goto-page page)
;;       )))
;; 
;; (defun org-redmine-store-link ()
;;   "Store a link to a docview buffer"
;;   (when (eq major-mode 'redmine-mode)
;;     ;; This buffer is in redmine mode
;;     (let* ((path redmine-last-visited-issue-directory)
;;            (issue-id (trim-whitespace (redmine-extract-thing-at-point redmine-issue-id-regex 1)))
;; 	   (issue-description (redmine-extract-thing-at-point redmine-issue-description-regex 1))
;;            (link (concat "elisp:(gtpmenu-redmine-goto-issue \"" path "\" \"" issue-id "\")"))
;;            (description ""))
;;       (org-store-link-props
;;        :type "redmine"
;;        :description (concat issue-id " " issue-description)
;;        :link link))))
;; 
;;;

