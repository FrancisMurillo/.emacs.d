;;; workgroups-layout.el --- A simple workgroups auto configure  -*- lexical-binding: t; -*-
;;
;; Filename: workgroups-layout.el
;; Description: A simple workgroups auto configure
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Mon Apr 17 19:40:30 2017 (+0800)
;; Version: 0.10
;; Package-Requires: ((emacs "24.4) (workgroups2 "0.1") (dash "0.1"))
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords: convenience
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

(require 'workgroups2)
(require 'dash)


;; Variables
(defvar workgroups-layout-layouts (list)
  "A list of workgroup configs.")

(defvar workgroups-layout-current-workgroup-config nil
  "Current workgroup config used.")

(defvar workgroups-layout-loaded (list)
  "Workgroups config loaded.
Used to guarantee `setup' is called only once.")

(defun* workgroups-layout-define-workgroup-layout
    (layout-name
     &key
     (setup #'ignore)
     (entry #'ignore)
     (exit #'ignore))
  (let* ((base-format "workgroups-layout-%s-%s-function")
         (setup-function-name
          (downcase (format base-format layout-name "setup")))
         (entry-function-name
          (downcase (format base-format layout-name "entry")))
         (exit-function-name
          (downcase (format base-format layout-name "exit")))
         (setup-function (intern setup-function-name))
         (entry-function (intern entry-function-name))
         (exit-function (intern exit-function-name)))
    (if (>= (length workgroups-layout-layouts) 10)
        (error "You already have 10 workgroup configurations already")
      (fset setup-function setup)
      (fset entry-function entry)
      (fset exit-function exit)
      (if (assoc layout-name workgroups-layout-layouts)
          (setcdr (assoc layout-name workgroups-layout-layouts)
                  (list setup-function
                        entry-function
                        exit-function))
        (add-to-list
         'workgroups-layout-layouts
         (list
          layout-name
          setup-function
          entry-function
          exit-function)
         t)))))


;; Api
(defun workgroups-layout-name (workgroup)
  "Get the layout name of a WORKGROUP."
  (nth 0 workgroup))

(defun workgroups-layout-setup-function (workgroup)
  "Get the setup function of a WORKGROUP."
  (nth 1 workgroup))


(defun workgroups-layout-entry-function (workgroup)
  "Get the entry function of a WORKGROUP."
  (nth 2 workgroup))

(defun workgroups-layout-exit-function (workgroup)
  "Get the exit function of a WORKGROUP."
  (nth 3 workgroup))


;; Workgroup specific
(defun workgroups-layout-workgroup-name (workgroup)
  "Get the name of a WORKGROUP."
  (wg-workgroup-name workgroup))

(defun workgroups-layout-create-default-workgroup (name)
  "Like `wg-make-and-add-workgroup' with NAME but guarantee it blank or empty."
  (wg-make-and-add-workgroup name t))

(defun workgroups-layout-current-workgroup-name ()
  "Get the current workgroup name."
  (when workgroups-mode
    (workgroups-layout-workgroup-name (wg-current-workgroup))))

(defun workgroups-layout-workgroups ()
  "Get the workgroups."
  (when workgroups-mode
    (wg-workgroup-list-or-error)))


;; Custom
(defun workgroups-layout-find-layout (workgroup-name)
  "Get the corresponding workgroup from the config by the WORKGROUP-NAME."
  (-first
   (lambda (workgroup-layout)
     (let ((layout-name (workgroups-layout-name workgroup-layout)))
       (string-equal layout-name workgroup-name)))
   workgroups-layout-layouts))

(defun workgroups-layout-current-layout ()
  "Get the current workgroup config."
  (workgroups-layout-find-layout (workgroups-layout-current-workgroup-name)))

(defun workgroups-layout-setup-and-entry ()
  "Initialize the workgroup per configuration."
  (condition-case ex
      (-if-let* ((config (workgroups-layout-current-layout))
                 (workgroup (wg-current-workgroup))
                 (config-name (workgroups-layout-name config))
                 (setup-function (workgroups-layout-setup-function config)))
          (unless (-contains-p workgroups-layout-loaded config-name)
            (message "Running setup for %s" config-name)
            (prog2
                (setq workgroups-layout-current-workgroup-config config)
                (funcall setup-function workgroup)
              (setq workgroups-layout-current-workgroup-config nil))
            (add-to-list 'workgroups-layout-loaded config-name))
        nil)
    ('error (message "Workgroup config setup error: %s" (error-message-string ex))))
  (condition-case ex
      (-if-let* ((config (workgroups-layout-current-layout))
                 (workgroup (wg-current-workgroup))
                 (entry-function (workgroups-layout-entry-function config)))
          (progn
            (message "Running entry for %s" (workgroups-layout-name config))
            (prog2
                (setq workgroups-layout-current-workgroup-config config)
                (funcall entry-function workgroup)
              (setq workgroups-layout-current-workgroup-config nil)))
        nil)
    ('error (message "Workgroup config entry error: %s" (error-message-string ex)))))

(defun workgroups-layout-exit ()
  "Cleanup for workgroup configuration."
  (condition-case ex
      (-if-let* ((config (workgroups-layout-current-layout))
                 (workgroup (wg-current-workgroup))
                 (exit-function (workgroups-layout-exit-function config)))
          (progn
            (message "Running exit for %s" (workgroups-layout-name config))
            (prog2
                (setq workgroups-layout-current-workgroup-config config)
                (funcall exit-function workgroup)
              (setq workgroups-layout-current-workgroup-config nil)))
        nil)
    ('error (message "Workgroup config exit error: %s" (error-message-string ex)))))


(defun workgroups-layout-configure ()
  "Create the workgroup configs based on `workgroups-layout-layouts'."
  (interactive)
  (if workgroups-mode
      (prog1
          (-map-indexed
           (lambda (index config)
             (lexical-let* ((config-name (workgroups-layout-name config))
                            (workgroups (workgroups-layout-workgroups))
                            (workgroup-index
                             (-find-index
                              (lambda (workgroup)
                                (string-equal (workgroups-layout-workgroup-name workgroup) config-name))
                              workgroups)))
               (cond
                ((null workgroup-index)
                 (message "Creating workspace %s" config-name)
                 (wg-swap-workgroups-in-workgroup-list
                  (workgroups-layout-create-default-workgroup config-name)
                  (nth index workgroups)))
                ((= workgroup-index index)
                 "NOOP: All is good here.")
                (t
                 (wg-swap-workgroups-in-workgroup-list
                  (nth index workgroups)
                  (nth workgroup-index workgroups))))))
           workgroups-layout-layouts)
        (message "Done auto configuring workgroups."))
    (message "Workgroup mode is not active.")))


(setq wg-open-this-wg (workgroups-layout-name (car workgroups-layout-layouts)))


;;;###autoload
(define-minor-mode workgroups-layout-mode
  "`workgroups-layout' minor mode for hooks."
  :lighter" workgroups-layout"
  :init-value nil
  :global t
  :keymap nil
  (if (symbol-value workgroups-layout-mode)
      (progn
        (add-hook 'wg-after-switch-to-workgroup-hook #'workgroups-layout-setup-and-entry)
        (add-hook 'wg-before-switch-to-workgroup-hook #'workgroups-layout-exit))
    (remove-hook 'wg-after-switch-to-workgroup-hook #'workgroups-layout-setup-and-entry)
    (remove-hook 'wg-before-switch-to-workgroup-hook #'workgroups-layout-exit)))


(provide 'workgroups-layout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workgroups-layout.el ends here
