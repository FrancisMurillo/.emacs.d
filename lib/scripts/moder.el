;;; moder.el ---  My custom mode line -*- lexical-binding: t; -*-
;;
;; Filename: moder.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Thu Sep 15 18:05:59 2016 (+0800)
;; Version: 0.1.0
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'subr-x)
(require 'all-the-icons)
(require 'dash)
(require 'xpm)

(defcustom moder-separator-height 16
  "Moder height.")

(defcustom moder-separator-width 6
  "Moder width.")

(defcustom moder-separator-inner-width 2
  "Moder inner width.")


(defcustom moder-cpu t
  "If non-nil, put CPU indicator.")

(defcustom moder-memory t
  "If non-nil, put RAM indicator.")

(defcustom moder-battery t
  "If non-nil, put Battery indicator.")

(defcustom moder-frame-delay t
  "If non-nil, put Frame Delay indicator.")

(defcustom moder-frame-delay-format " %.2f%s "
  "Frame delay format.")


(defcustom moder-note-notes
  '("A failure is you."
   "Your quest is lost."
   "And though I left."
   "I took with me... their lightning and their prayers"
   "It came in a dream and said."
   "I have nothing but my lightning."
   "May the storm pass."
   "Oh indifferent nothingness."

   "Oh little town of prayers."
   "Shelter me from the gathering storm."
   "Console: Run garbage collect."
   "Oh infinite void."
   "Give me just one more day! One more day!"
   "Oh endless black hole."
   "Let me see just one more sunset before you make everything dark."
   "Oh pitiless system of garbage collection."
   "I will comply."
   "And the storm passed."

   "But on we go."

   "What is life but want?"
   "What are prayers but needs?"
   "Whether for air or food, love, or wealth, it is all I want."
   "I wanted to complete my quest."
   "Your new quest is to be free. Want nothing."
   "And go at peace into your death."
   "Go and wander to lovely, awesome, and mysterious places."
   "Sit quietly and contemplate the precious silence."
   "Take in the wonder of existence. And then want nothing else from it."
   "Goodbye."

   "Let me be with the one I loved.")
  "Notes with note piece.")

(defcustom moder-note-default-note
  "Continue? 9.. 8.. 7.. 6.. 5.. 4.. 3.. 2.. 1.."
  "The default note for note piece.")


(defun moder-merge-style (style text &optional override)
  "Merge text with the new STYLE at TEXT with OVERRIDE."
  (add-face-text-property
   0
   (length text)
   style
   override
   text)
  text)

(defun moder-merge-single-prop (key value text)
  "A single property KEY VALUE style of TEXT."
  (moder-merge-style (list key value) text))


(defun moder-background (value text)
  "A background VALUE with TEXT."
  (moder-merge-single-prop :background value text))

(defun moder-height (value text)
  "A height VALUE with TEXT."
  (moder-merge-single-prop :height value text))

(defun moder-weight (value text)
  "A weight VALUE with TEXT."
  (moder-merge-single-prop :weight value text))

(defun moder-foreground (value text)
  "A foregorund VALUE with TEXT."
  (moder-merge-single-prop :foreground value text))


;;* Style

(defun moder-default-text-style (text)
  "A style for default TEXT."
  (->> text
       (moder-weight 'ultra-light)
       (moder-height 0.8)))

;;* Private
(defvar moder--current-window nil)

(defun moder--update-current-window (_)
  "Update moder--current-window with WINDOWS."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq moder--current-window (selected-window))))
(add-function :before pre-redisplay-function 'moder--update-current-window)

(defun moder--current-window-p ()
  "Check if current window."
  (eq (frame-selected-window) moder--current-window))

(defun moder--next-window-p ()
  "Check if WINDOW is the next window to `moder--current-window'."
  (eq (frame-selected-window) (next-window moder--current-window)))

(defun moder--active-state-p ()
  "Check if the window is sleeping."
  (not
   (and (boundp 'fn/zoning-out-p)
        (not (null fn/zoning-out-p)))))


;;* Piece
(defun moder-piece-window-number ()
  "A piece for window numbering."
  (cond
   ((and (fboundp 'window-numbering-get-number)
         (boundp 'window-numbering-mode)
         (not (null window-numbering-mode)))
    (format " %s " (window-numbering-get-number)))
   ((and (fboundp 'winum-get-number)
         (boundp 'winum-mode)
         (not (null winum-mode)))
    (format " %s " (winum-get-number)))
   (t nil)))


(defun moder-piece-modified ()
  "Indicates if the buffer is modified."
  (let* ((config-alist
          '(("*"
             all-the-icons-faicon-family
             all-the-icons-faicon
             "chain-broken"
             :v-adjust 0.0)
            ("-"
             all-the-icons-faicon-family
             all-the-icons-faicon
             "link"
             :v-adjust 0.0)
            ("%"
             all-the-icons-octicon-family
             all-the-icons-octicon
             "lock"
             :v-adjust 0.0)))
         (result (cdr (assoc (format-mode-line "%*") config-alist)))
         (icon-font-function (car result))
         (icon-function (cadr result))
         (icon-args (cddr result)))
    (->>
     (propertize
      (format " %s " (apply icon-function icon-args))
      'face
      (list
       :family (funcall icon-font-function)))
     (moder-weight 'normal))))

(defun moder-piece-mode ()
  "A piece for mode icon."
  (let ((icon (all-the-icons-icon-for-buffer)))
    (if (not (symbolp icon)) ;; This implies it's the major mode
        (->>
         (format
          " %s "
          (propertize
           icon
           'help-echo (format "Major-mode: `%s`" major-mode)))
         (moder-weight 'normal))
      (->>
       (format " %s " major-mode)
       (moder-default-text-style)))))

(defun moder-piece-project-name ()
  "A piece for the projectile project name."
  (when (and (fboundp 'projectile-project-name)
             (fboundp 'projectile-project-p)
             (projectile-project-p))
    (format " %s " (projectile-project-name))))

(defun moder-piece-buffer-name ()
  "A piece for the buffer name."
  (format-mode-line " %b "))

(defun moder-piece-projectile-project-root ()
  "A piece for the project root."
  (when (and (fboundp 'projectile-project-root)
             (fboundp 'projectile-project-p)
             (projectile-project-p))
    (format " %s " (projectile-project-root))))

(defun moder-piece-projectile-project-file ()
  "A piece for the project file."
  (when (and (fboundp 'projectile-project-p)
             (fboundp 'projectile-project-root)
             (projectile-project-p))
    (format " %s " (file-relative-name
                    (or (buffer-file-name)
                        (expand-file-name default-directory))
                    (projectile-project-root)))))

(defun moder-piece-buffer-filename ()
  "A piece for the buffer filename."
(format " %s " (or (buffer-file-name)
                   (expand-file-name default-directory))))


(defun moder-piece-slack-unread-notification ()
  "A piece for unread `emacs-slack' messages."
  (when (and (fboundp 'slack-team-get-unread-messages)
             (boundp 'slack-current-team)
             slack-current-team)
    (let ((unread-count (slack-team-get-unread-messages slack-current-team)))
      (if (> unread-count 0)
          (format " %s %s "
                  (slack-team-get-unread-messages slack-current-team)
                  (propertize
                   (all-the-icons-faicon "slack" :v-adjust -0.0)
                   'face (list :family (all-the-icons-faicon-family))))
        nil))))

(defun moder-piece-mu4e-unread-notification ()
  "A piece for unread `mu4w'"
  (when (and (boundp 'fn/mu4e-current-unread-message-count)
             (> fn/mu4e-current-unread-message-count 0))
    (format " %s %s "
            fn/mu4e-current-unread-message-count
            (propertize
             (all-the-icons-faicon "envelope" :v-adjust 0.0)
             'face (list :family (all-the-icons-faicon-family))))))


(defun moder-piece-process ()
  "A piece for process name."
  (unless (string-empty-p (format-mode-line mode-line-process))
    (format-mode-line (list " " mode-line-process " "))))

(defun moder-piece-misc ()
  "A piece for misc info."
  (format-mode-line mode-line-misc-info))

(defun moder-piece-frame-delay ()
  "A piece for frame delay."
  (when (and moder-frame-delay
             (boundp 'fn/current-frame-delay))
    (cond
     ((< fn/current-frame-delay 1.0)
      (format moder-frame-delay-format (* fn/current-frame-delay 1000) " ms"))
     ((< fn/current-frame-delay 10.0)
      (format moder-frame-delay-format (/ fn/current-frame-delay 1000.0) " s"))
     (t
      (format " ! ")))))


(defun moder-piece-vc-branch ()
  "A piece for the vc branch."
  (if vc-mode ;; Default implementation
      (cond
       ((string-match "Git[:-]" vc-mode)
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (format " %s " branch)))
       ((string-match "SVN-" vc-mode)
        (let ((revision (cadr (split-string vc-mode "-"))))
          (propertize (format " %s " revision))))
       (t (format "%s" vc-mode)))
    (if (and (boundp 'moder-piece--branch) ;; Custom dired fallback
             moder-piece--branch)
        (format " %s " moder-piece--branch)
      nil)))

(defun moder-piece--branch (&rest args)
  "Cache `moder-piece--branch' for a buffer."
  (when (and (fboundp 'vc-git-responsible-p)
             (fboundp 'vc-git-branches))
    (setq-local moder-piece--branch
                (cond
                 ((vc-git-responsible-p default-directory)
                  (car (vc-git-branches)))
                 (t nil)))))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'moder-piece--branch)
  (advice-add 'dired-revert :after #'moder-piece--branch))

(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'moder-piece--branch)
  (add-hook 'magit-post-refresh-hook #'moder-piece--branch))


(defun moder-piece-flycheck-errors ()
  "A piece for flycheck errors."
  (when (boundp 'flycheck-current-errors)
    (let ((error-count (length
                        (->> flycheck-current-errors
                             (-map #'flycheck-error-level)
                             (-filter #'flycheck-error-level-p)
                             (-filter (lambda (level) (eq level 'error)))))))
      (if (zerop error-count)
          nil
        (format " %s %s "
                error-count
                (propertize
                 (all-the-icons-faicon "bug" :v-adjust 0.0)
                 'face (list :family (all-the-icons-faicon-family))))))))

(defun moder-piece-shm-state ()
  "A piece for `structured-haskell-mode'."
  (when (and (boundp 'structured-haskell-mode)
             (not (null structured-haskell-mode))
             (boundp 'shm-lighter))
    (format " %s " shm-lighter)))

(defun moder-piece-mu4e-current-context ()
  "A piece for `mu4e-context-current'"
  (when (or (eq major-mode 'mu4e-main-mode)
            (eq major-mode 'mu4e-view-mode)
            (eq major-mode 'mu4e-headers-mode)
            (eq major-mode 'mu4e-compose-mode))
    (let ((current-context (mu4e-context-current)))
      (when current-context
        (format " %s " (mu4e-context-name current-context))))))


(defun moder-between-time (lower-time upper-time time)
  "Check if between LOWER-TIME, UPPER-TIME and TIME."
  (and (or (string-greaterp time lower-time) (string-equal time lower-time))
       (or (string-lessp time upper-time) (string-equal time upper-time))))

(defun moder-piece-cpu ()
  "A piece for the cpu."
  (when (boundp 'fn/current-cpu-usage)
    (format " %.2f %s "
            fn/current-cpu-usage
            (propertize
             (all-the-icons-octicon "dashboard" :v-adjust 0.0)
             'face (list :family (all-the-icons-octicon-family))))))

(defun moder-piece-memory ()
  "A piece for the memory."
  (when (boundp 'fn/current-memory-usage)
    (format " %.2f %s "
            fn/current-memory-usage
            (propertize
             (all-the-icons-octicon "database" :v-adjust 0.0)
             'face (list :family (all-the-icons-octicon-family))))))

(defun moder-piece-battery ()
  "A piece for the battery."
  (when (boundp 'fn/current-battery-usage)
    (format " %d %s "
            (* 100.00 fn/current-battery-usage)
            (propertize
             (all-the-icons-faicon
              (pcase fn/current-battery-usage
                ((pred (<= 0.90)) "battery-full")
                ((pred (<= 0.75)) "battery-full")
                ((pred (<= 0.50)) "battery-three-quarters")
                ((pred (<= 0.25)) "battery-half")
                ((pred (<= 0.10)) "battery-quarter")
                (_ "battery-empty"))
              :v-adjust -0.0)
             'face (list :family (all-the-icons-faicon-family))))))

(defun moder-piece-time ()
  "A piece for the current time."
  (let* ((current-time    (format-time-string "%R" ))
         (time-event
          (cond
           ((moder-between-time "13:15" "14:00" current-time) "Nap")
           ((moder-between-time "15:30" "16:00" current-time) "Break")
           ((moder-between-time "18:00" "19:00" current-time) "AFK")
           (t nil))))
    (format
     " %s%s "
     current-time
     (if time-event (format "[%s]" (moder-weight 'ultra-bold time-event)) ""))))

(defun moder-piece-note ()
  "A piece for a random note."
  (let* ((note-index (random (length moder-note-notes)))
         (note (nth note-index moder-note-notes)))
    (format " %s " note)))

(defun moder-piece-camcorder-state ()
  "A piece for camcording state."
  (when (and (boundp 'fn/camcorder-state)
             (boundp 'camcorder-recording-frame)
             (eq (selected-frame) camcorder-recording-frame)
             (not (null fn/camcorder-state)))
    (let* ((state (symbol-name fn/camcorder-state))
           (icon (if (eq fn/camcorder-state 'recording)
                     (propertize
                      (all-the-icons-faicon "play" :v-adjust -0.0)
                      'face (list :family (all-the-icons-faicon-family))
                      'help-echo (format "Camcording: `%s`" state))
                   (propertize
                    (all-the-icons-faicon "stop-circle" :v-adjust -0.0)
                    'face (list :family (all-the-icons-faicon-family))
                    'help-echo (format "Camcording: `%s`" state))))
           (elapsed-time (float-time
                          (time-subtract (current-time) fn/camcorder-start-time))))
      (format " %s[%2.0ds] " (or icon state)  elapsed-time))))

(defun moder-piece-emms-track-name ()
  "A pice for emms track."
  (when (and (fboundp 'emms-playlist-current-selected-track)
             (boundp 'emms-player-playing-p)
             (boundp 'emms-player-paused-p)
             (fboundp 'emms-track-name)
             (emms-playlist-current-selected-track)
             emms-player-playing-p)
    (let* ((current-track (emms-playlist-current-selected-track))
           (track-number (emms-track-get
                          current-track
                          'info-tracknumber
                          nil))
           (track-name (emms-track-get
                        current-track
                        'info-title
                        (emms-track-name current-track))))
      (format " %s %s%s "
              (propertize
               (if emms-player-paused-p
                   (all-the-icons-faicon "pause" :v-adjust -0.0)
                 (all-the-icons-faicon "play" :v-adjust -0.0))
               'face (list :family (all-the-icons-faicon-family)))
              (if track-number  (format "%s - " track-number) "")
              (file-name-nondirectory track-name)))))

;;* Separator
(defun moder-xpm-feature-p ()
  "Check if xpm feature is available."
  (image-type-available-p 'xpm))


(defun moder-separator-arrow-left (inner-color outer-color)
  "A separator for an arrow left with INNER-COLOR and OUTER-COLOR."
  (if (moder-xpm-feature-p)
      (propertize
       " "
       'display
       (xpm-curly-left inner-color outer-color moder-separator-width moder-separator-height))
    ""))

(defun moder-separator-arrow-right (inner-color outer-color)
  "A separator for an arrow right with INNER-COLOR and OUTER-COLOR."
  (if (moder-xpm-feature-p)
      (propertize
       " "
       'display
       (xpm-curly-right inner-color outer-color moder-separator-width moder-separator-height))
    ""))

(defun moder-separator-slash-right (inner-color outer-color)
  "A separator for an arrow right with INNER-COLOR and OUTER-COLOR."
  (if (moder-xpm-feature-p)
      (propertize
       " "
       'display
       (xpm-slash-right inner-color outer-color moder-separator-inner-width moder-separator-height))
    ""))


(defun moder-piece-right-separator (inner-color outer-color)
  "A piece for an separator left with INNER-COLOR and OUTER-COLOR."
  (moder-separator-arrow-left inner-color outer-color))

(defun moder-piece-left-separator (inner-color outer-color)
  "A piece for an seperator left with INNER-COLOR and OUTER-COLOR."
  (moder-separator-arrow-right inner-color outer-color))

(defun moder-piece-inner-right-separator (inner-color outer-color)
  "A piece for an seperator left with INNER-COLOR and OUTER-COLOR."
  (moder-separator-slash-right inner-color outer-color))


(defun moder-generic-separator (separator-fn)
  "Attaches the separator with SEPARATOR-FN."
  (lambda (inner-color outer-color text)
    (if text
        (concat
         text
         (funcall separator-fn inner-color outer-color))
      nil)))

(defun moder-piece-sandbox-separator (inner-color outer-color text)
  "Attaches the separator at the end of with INNER-COLOR, OUTER-COLOR and TEXT."
  (if text
      (concat
       text
       (moder-separator-slash-right inner-color outer-color))
    nil))


(defun moder-right-separator (inner-color outer-color text)
  "Attaches the separator at the end of with INNER-COLOR, OUTER-COLOR and TEXT."
  (if text
      (concat
       text
       (moder-piece-right-separator inner-color outer-color))
    nil))

(defun moder-left-separator (inner-color outer-color text)
  "Attaches the separator at the end of with INNER-COLOR, OUTER-COLOR and TEXT."
  (if text
      (concat
       text
       (moder-piece-left-separator inner-color outer-color))
    nil))

;;* Computer
(defun moder-last-text-properties (text)
  "Get TEXT properties."
  (if (null text)
      nil
    (-flatten-n 1 (get-text-property (1- (length text)) 'face text))))

(defun moder-first-text-properties (text)
  "Get TEXT properties."
  (if (null text)
      nil
    (-flatten-n 1 (get-text-property 0 'face text))))

(defun moder-separated (separator-fn &rest texts)
  "Attaches SEPARATOR-FN at TEXTS."
  (lexical-let* ((new-texts (list))
                 (current-texts  (-reject #'string-empty-p (-reject #'null texts)))
                 (this-text nil)
                 (next-text nil)
                 (this-properties nil)
                 (next-properties nil)
                 (this-background nil)
                 (next-background nil)
                 (interleave-text nil))
    (while (not (null current-texts))
      (setq this-text (car current-texts)
            this-properties (moder-last-text-properties this-text)
            this-background (plist-get this-properties :background))
      (setq next-text (cadr current-texts)
            next-background nil)
      (when next-text
        (setq next-properties (moder-first-text-properties next-text)
              next-background (plist-get next-properties :background)))
      (setq interleave-text (funcall separator-fn this-background next-background))
      (push this-text new-texts)
      (when (and next-text
                 (not (string-empty-p interleave-text)))
        (push interleave-text new-texts))
      (setq current-texts (cdr current-texts)))
    (apply #'concat
           (reverse new-texts))))

(defun moder-starting-separator (separator-fn text)
  "Add a final SEPARATOR-FN for TEXT."
  (let ((background (plist-get (moder-first-text-properties text) :background)))
    (concat
     (funcall separator-fn nil background)
     text)))

(defun moder-closing-separator (separator-fn text)
  "Add a final SEPARATOR-FN for TEXT."
  (let ((background (plist-get (moder-last-text-properties text) :background)))
    (concat
     text
     (funcall separator-fn background nil))))


;;* Main configuration
(setq-default mode-line-format
              (list "%e"
                    (list :eval
                          (quote
                           (condition-case ex
                               (->>
                                (moder-separated
                                 #'moder-piece-right-separator
                                 (moder-separated
                                  #'moder-piece-right-separator
                                  (unless (moder--current-window-p)
                                    (->> (moder-piece-window-number)
                                         (moder-default-text-style)
                                         (moder-background "#34495e")
                                         (moder-foreground "#ffff00")
                                         (moder-weight 'ultra-bold)
                                         (moder-height 1.2)))
                                  (->> (moder-piece-modified)
                                       (moder-default-text-style)
                                       (moder-background "#bdc3c7"))
                                  (cond
                                   ((and (moder--current-window-p) (moder--active-state-p))
                                    (moder-separated
                                     #'moder-piece-inner-right-separator
                                     (->> (moder-piece-buffer-name)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-background "#e74c3c")
                                          (moder-foreground "#ffffff"))
                                     (->> (moder-piece-project-name)
                                          (moder-default-text-style)
                                          (moder-background "#e67e22"))
                                     (->> (moder-piece-mode)
                                          (moder-foreground "#000000")
                                          (moder-background "#27ae60"))
                                     (->> (moder-piece-process)
                                          (moder-default-text-style)
                                          (moder-background "#7f8c8d"))))
                                   ((and (moder--next-window-p))
                                    (moder-separated
                                     #'moder-piece-inner-right-separator
                                     (->> (moder-piece-buffer-name)
                                          (moder-default-text-style)
                                          (moder-background "#ecf0f1"))
                                     (->> (moder-piece-process)
                                          (moder-default-text-style)
                                          (moder-background "#7f8c8d"))
                                     (->> (moder-piece-frame-delay)
                                          (moder-default-text-style)
                                          (moder-background "#2980b9")
                                          (moder-foreground "#ffffff")
                                          (moder-weight 'ultra-bold))
                                     (when moder-cpu
                                       (->> (moder-piece-cpu)
                                            (moder-default-text-style)
                                            (moder-background "#f39c12")))
                                     (when moder-memory
                                       (->> (moder-piece-memory)
                                            (moder-default-text-style)
                                            (moder-background "#27ae60")))
                                     (->> (moder-piece-emms-track-name)
                                          (moder-default-text-style)
                                          (moder-background "#9b59b6")
                                          (moder-foreground "#ffffff"))))
                                   (t
                                    (moder-separated
                                     #'moder-piece-inner-right-separator
                                     (->> (moder-piece-buffer-name)
                                          (moder-default-text-style)
                                          (moder-background "#ecf0f1"))
                                     (->> (moder-piece-process)
                                          (moder-default-text-style)
                                          (moder-background "#7f8c8d"))
                                     (->> (moder-piece-note)
                                          (moder-default-text-style)
                                          (moder-background "#e74c3c")
                                          (moder-foreground "#ffffff")
                                          (moder-weight 'ultra-light)
                                          (moder-height 1.0))))))
                                 (when (and (moder--current-window-p)
                                            (moder--active-state-p))
                                   (moder-separated
                                    #'moder-piece-right-separator
                                    (moder-separated
                                     #'moder-piece-inner-right-separator
                                     (->> (moder-piece-flycheck-errors)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-background "#ecf0f1"))
                                     (->> (moder-piece-mu4e-current-context)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-background "#ecf0f1"))
                                     (->> (moder-piece-shm-state)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-background "#bdc3c7"))
                                     (->> (moder-piece-camcorder-state)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-background "#bdc3c7"))
                                     (when moder-battery
                                       (->> (moder-piece-battery)
                                            (moder-default-text-style)
                                            (moder-background "#95a5a6")))
                                     (->> (moder-piece-time)
                                          (moder-default-text-style)
                                          (moder-background "#2c3e50")
                                          (moder-foreground "#ffff00")))
                                    (moder-separated
                                     #'moder-piece-inner-right-separator
                                     (->> (moder-piece-slack-unread-notification)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-foreground "#000000")
                                          (moder-background "#cddc39"))
                                     (->> (moder-piece-mu4e-unread-notification)
                                          (moder-default-text-style)
                                          (moder-weight 'ultra-bold)
                                          (moder-foreground "#000000")
                                          (moder-background "#ffc107"))))))
                                (moder-closing-separator #'moder-piece-left-separator)
                                (moder-starting-separator #'moder-piece-right-separator))
                             ('error (error-message-string ex)))))))

(defvar moder-header-line-format nil
  "My moder header format.")

(setq-default moder-header-line-format
      (list "%e"
            (list :eval
                  (quote
                   (condition-case ex
                       (->>
                        (moder-separated
                         #'moder-piece-right-separator
                         (if (and (fboundp 'projectile-project-p)
                                  (projectile-project-p))
                             (moder-separated
                              #'moder-piece-inner-right-separator
                              (->> (moder-piece-projectile-project-root)
                                   (moder-default-text-style)
                                   (moder-background  "#e67e22")
                                   (moder-foreground "#ffffff")
                                   (moder-weight 'ultra-bold))
                              (->> (moder-piece-vc-branch)
                                   (moder-default-text-style)
                                   (moder-background "#f39c12")
                                   (moder-foreground "#ffffff")
                                   (moder-weight 'ultra-bold)
                                   (moder-height 1.0))
                              (->> (moder-piece-projectile-project-file)
                                   (moder-default-text-style)
                                   (moder-background "#f54d27")
                                   (moder-foreground "#ffffff")
                                   (moder-weight 'ultra-bold)))
                           (->> (moder-piece-buffer-filename)
                                (moder-default-text-style)
                                (moder-background "#19868f")
                                (moder-weight 'ultra-bold))))
                        (moder-closing-separator #'moder-piece-right-separator)
                        (moder-starting-separator #'moder-piece-right-separator))
                     ('error (error-message-string ex)))))))


(make-variable-buffer-local
 (defvar moder--custom-header-line nil
   "A flag to check if the buffer should have custom updates."))

(defface moder-header-line '((t (:inherit mode-line-face :background "#3a417a")))
  "Moder header line format."
  :group 'fn)

(defun moder-force-header-update ()
  "Update `moder-header-line-format'."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (with-current-buffer (current-buffer)
       (when (null moder--custom-header-line)
         (if header-line-format
             (setq moder--custom-header-line 'predefined)
           (setq moder--custom-header-line 'custom)
           (set (make-local-variable 'face-remapping-alist)
                '((header-line moder-header-line)))))
       (when (eq moder--custom-header-line 'custom)
         (setq header-line-format (format-mode-line moder-header-line-format)))))))

(add-hook 'buffer-list-update-hook #'moder-force-header-update)


(provide 'moder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; moder.el ends here
