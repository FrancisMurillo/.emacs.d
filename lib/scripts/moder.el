;;; moder.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: moder.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Thu Sep 15 18:05:59 2016 (+0800)
;; Version:
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

(require 'all-the-icons)

(require 'dash)

(require 'xpm)

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

(defun moder--active-state-p ()
  "Check if the window is sleeping."
  (not
   (and (boundp fn/zoning-out-p)
      (not (null fn/zoning-out-p)))))


;;* Piece
(defun moder-piece-window-number ()
  "A piece for window numbering."
  (if (and (fboundp 'window-numbering-get-number)
         (boundp 'window-numbering-mode)
         (not (null window-numbering-mode)))
      (format " %s " (window-numbering-get-number))))

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
           'help-echo (format "Major-mode: `%s`" major-mode)
           'face (list :family (all-the-icons-icon-family-for-buffer))))
         (moder-weight 'normal))
      (->>
       (format " %s " major-mode)
       (moder-default-text-style)))))

(defun moder-piece-workgroup-name ()
  "A piece for the workgroup name."
  (when (and (fboundp 'wg-current-workgroup)
           (fboundp 'wg-workgroup-name)
           (fboundp 'workgroups-mode)
           (not (null workgroups-mode)))
    (format " %s " (wg-workgroup-name (wg-current-workgroup)))))

(defun moder-piece-project-name ()
  "A piece for the projectile project name."
  (when (fboundp 'projectile-project-name)
    (format " %s " (projectile-project-name))))

(defun moder-piece-buffer-name ()
  "A piece for the buffer name."
  (format-mode-line " %b "))

(defun moder-piece-process ()
  "A piece for process name."
  (format-mode-line mode-line-process))

(defun moder-piece-misc ()
  "A piece for misc info."
  (format-mode-line mode-line-misc-info))

(defun moder-piece-frame-delay ()
  "A piece for frame delay."
  (when (boundp 'fn/current-frame-delay)
    (if (<= fn/current-frame-delay 10.0)
        (format " %.3fms "  fn/current-frame-delay)
      (format " !ms "))))

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

(defun moder-piece-time ()
  "A piece ofr the current time."
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


;;* Separator
(defun moder-separator-arrow-left (inner-color outer-color)
  "A separator for an arrow left with INNER-COLOR and OUTER-COLOR."
  (propertize
   " "
   'display
   (xpm-arrow-left inner-color outer-color 8 16)))

(defun moder-separator-arrow-right (inner-color outer-color)
  "A separator for an arrow right with INNER-COLOR and OUTER-COLOR."
  (propertize
   " "
   'display
   (xpm-arrow-right inner-color outer-color 8 16)))

(defun moder-separator-slash-right (inner-color outer-color)
  "A separator for an arrow right with INNER-COLOR and OUTER-COLOR."
  (propertize
   " "
   'display
   (xpm-slash-right inner-color outer-color 2 16)))


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
      (when next-text
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
                 (->> (moder-piece-window-number)
                      (moder-default-text-style)
                      (moder-background "#34495e")
                      (moder-foreground "#ffff00")
                      (moder-weight 'ultra-bold)
                      (moder-height 1.2))
                 (->> (moder-piece-modified)
                      (moder-default-text-style)
                      (moder-background "#bdc3c7"))
                 (if (and (moder--current-window-p) (moder--active-state-p))
                     (moder-separated
                      #'moder-piece-inner-right-separator
                      (->> (moder-piece-buffer-name)
                           (moder-default-text-style)
                           (moder-weight 'ultra-bold)
                           (moder-background "#e74c3c"))
                      (->> (moder-piece-project-name)
                           (moder-default-text-style)
                           (moder-background "#e67e22"))
                      (->> (moder-piece-workgroup-name)
                           (moder-default-text-style)
                           (moder-background "#f1c40f"))
                      (->> (moder-piece-mode)
                           (moder-background "#27ae60")))
                   (->> (moder-piece-buffer-name)
                        (moder-default-text-style)
                        (moder-background "#ecf0f1"))))
                (->> (moder-piece-process)
                     (moder-default-text-style)
                     (moder-background "#7f8c8d"))
                (when (and (moder--current-window-p) (moder--active-state-p))
                  (moder-separated
                   #'moder-piece-inner-right-separator
                   (->> (moder-piece-frame-delay)
                        (moder-default-text-style)
                        (moder-background "#9b59b6"))
                   (->> (moder-piece-cpu)
                        (moder-default-text-style)
                        (moder-background "#f1c40f"))
                   (->> (moder-piece-memory)
                        (moder-default-text-style)
                        (moder-background "#d35400"))
                   (->> (moder-piece-time)
                        (moder-default-text-style)
                        (moder-foreground "#ffff00")
                        (moder-background "#2c3e50")))))
               (moder-closing-separator #'moder-piece-left-separator)
               (moder-starting-separator #'moder-piece-right-separator))
            ('error (error-message-string ex)))))))


(provide 'moder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; moder.el ends here