;;; promise.el --- A simple promise library for my Emacs configuration  -*- lexical-binding: t; -*-
;;
;; Filename: promise.el
;; Description: A simple promise library for my Emacs configuration
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Sat Aug 27 18:59:25 2016 (+0800)
;; Version: 0.10
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

(require 'cl-lib)


;;* Private
(defun promise--make (state &optional value fulfilled-hooks rejected-hooks)
  "Create a promise with STATE, VALUE, FULFILLED-HOOKS and REJECTED-HOOKS."
  (list state value fulfilled-hooks rejected-hooks))

(defun promise--state (promise)
  "Get the state of a PROMISE, not to be used externally.  Mainly for unit testing."
  (car promise))


;;* Interface
(defun promise (executor)
  "Create a promise that is handled by an EXECUTOR."
  (letrec
      ((promised nil)
       (in-context t)
       (fulfiller
        (lambda (value)
          (pcase-let ((`(,state ,_ ,_ ,_) promised))
            (when (eq state 'pending)
              (setf (nth 0 promised) 'fulfilled)
              (setf (nth 1 promised) value)
              (unless in-context
                (funcall run-hooks))))))
       (rejector
        (lambda (reason)
          (pcase-let ((`(,state ,_ ,_ ,_) promised))
            (when (eq state 'pending)
              (setf (nth 0 promised) 'rejected)
              (setf (nth 1 promised) reason)
              (unless in-context
                (funcall run-hooks))))))
       (run-hooks
        (lambda ()
          (pcase-let ((`(,state ,promised-value ,fulfilled-hooks ,rejected-hooks) promised))
            (if (eq state 'pending)
                nil
              (if (promise-p promised-value)
                  (promise-then
                   promised-value
                   (lambda (value)
                     (funcall fulfiller value))
                   (lambda (value)
                     (funcall rejector value))))
              (pcase state
                (`fulfilled (mapc (lambda (hook) (funcall hook promised-value))
                                  fulfilled-hooks))
                (`rejected (mapc (lambda (hook) (funcall hook promised-value))
                                 rejected-hooks))))))))
    (setq promised (promise--make 'pending nil (list) (list)))
    (funcall executor fulfiller rejector)
    (setq in-context nil)
    (funcall run-hooks)
    promised))

(defun promise-p (value)
  "Check if VALUE is a promise."
  (pcase value
    (`(pending ,_ ,_ ,_) t)
    (`(fulfilled ,_ ,_ ,_) t)
    (`(rejected ,_ ,_ ,_) t)
    (_ nil)))

(defun promise-then (promise &optional fulfilled rejected)
  "Add a hook into a PROMISE with FULFILLED and REJECTED resolution."
  (promise
   (lambda (res rej)
     (lexical-let
         ((wrapped-resolved
           (lambda (value)
             (let ((new-value (funcall fulfilled value)))
               (if (promise-p new-value)
                   (promise-then
                    new-value
                    (lambda (newer-value)
                      (funcall res newer-value))
                    (lambda (newer-value)
                      (funcall rej newer-value)))
                 (funcall res new-value)))))
          (wrapped-rejected
           (lambda (value)
             (let ((new-value (funcall rejected value)))
               (if (promise-p new-value)
                   (promise-then
                    new-value
                    (lambda (newer-value)
                      (funcall res newer-value))
                    (lambda (newer-value)
                      (funcall rej newer-value)))
                 (funcall rej new-value))))))
       (pcase-let
           ((`(,state ,promise-value ,_ ,_) promise))
         (pcase state
           (`fulfilled
            (when (functionp fulfilled)
              (funcall wrapped-resolved promise-value)))
           (`rejected
            (when (functionp rejected)
              (funcall wrapped-rejected promise-value)))
           (`pending
            (when (functionp fulfilled)
              (push wrapped-resolved (nth 2 promise)))
            (when (functionp rejected)
              (push wrapped-rejected (nth 3 promise))))))))))


(defun promise-all (&rest promises)
  "Create a promise that is fulfilled when all PROMISES are fulfilled and rejected when any of it is rejected."
  (promise
   (lambda (fulfiller rejector)
     (lexical-let* ((fulfiller fulfiller)
                    (rejector rejector)
                    (fulfilled-promises 0)
                    (values (-repeat (length promises) nil ))
                    (all-fulfilled
                     (lambda (value index)
                       (setq fulfilled-promises (1+ fulfilled-promises)
                             values (-replace-at index value values))
                       (when (= fulfilled-promises (length promises))
                         (funcall fulfiller values))))
                    (any-rejected
                     (lambda (reason)
                       (funcall rejector reason))))
       (-map-indexed (lambda (index promise)
                       (lexical-let ((index index))
                         (promise-then promise
                                       (lambda (value)
                                         (funcall all-fulfilled value index))
                                       (lambda (reason)
                                         (funcall any-rejected reason)))))
                     promises)))))

(defun promise-idle-delay (delay promise)
  "Wrap a promise with a delay so that it would not block the interface
 by DELAY and PROMISE."
  (promise-then
   promise
   (lambda (value)
     (promise
      (lambda (res _)
        (run-with-idle-timer
         delay
         nil
         (lambda ()
           (funcall res value))))))))


(defun promise-all-features (&rest features)
  "Create a promise that is fulfilled when all FEATURES are loaded.  Useful when loading packages with dependencies and the reason this library is done."
  (apply
   #'promise-all
   (mapcar
    (lambda (feature)
      (lexical-let ((feature feature))
        (promise
         (lambda (fulfiller _)
           (with-eval-after-load feature
             (funcall fulfiller feature))))))
    features)))


(provide 'promise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promise.el ends here
