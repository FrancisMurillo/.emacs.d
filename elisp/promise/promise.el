;;; package --- Summary

;;; Commentary:

;;; Packages

;; -*- lexical-binding: t; -*-

(require 'dash)

;;; Code:

(defun p--promise (state &optional value fulfilled-hooks rejected-hooks)
  "Create a promise type."
  (list state value fulfilled-hooks rejected-hooks))

(defun p-make-promise (executor)
  "Create a promise that is handled by an executor."
  (lexical-let* ((promised nil)
                 (run-hooks (lambda ()
                              (pcase-let ((`(,state ,promised-value ,fulfilled-hooks ,rejected-hooks) promised))
                                (pcase state
                                  (`pending nil)
                                  (`fulfilled (mapc (lambda (hook) (funcall hook promised-value))
                                                    fulfilled-hooks))
                                  (`rejected (mapc (lambda (hook) (funcall hook promised-value))
                                                   rejected-hooks))))))
                 (in-context t)
                 (fulfiller (lambda (value)
                              (pcase-let ((`(,state ,promised-value ,_ ,_) promised))
                                (when (eq state 'pending)
                                  (setf (nth 0 promised) 'fulfilled)
                                  (setf (nth 1 promised) value)
                                  (unless in-context
                                    (funcall run-hooks))))))
                 (rejector (lambda (reason)
                             (pcase-let ((`(,state ,promised-value ,_ ,_) promised))
                               (when (eq state 'pending)
                                 (setf (nth 0 promised) 'rejected)
                                 (setf (nth 1 promised) reason)
                                 (unless in-context
                                   (funcall run-hooks)))))))
    (setq promised (p--promise 'pending nil (list) (list)))
    (funcall executor fulfiller rejector)
    (setq in-context nil)
    (funcall run-hooks)
    promised))

(defun p-promise-p (value)
  "Check if value is a promise"
  (pcase value
    (`(`pending `nil ,_ ,_) t)
    (`(`fulfilled ,_ ,_ ,_) t)
    (`(`rejected ,_ ,_ ,_) t)
    (_ nil)))


(defun p-then (promise &optional fulfilled rejected)
  "Add a hook into a promise resolution"
  (pcase-let ((`(,state ,promise-value ,fulfilled-hooks ,rejected-hooks) promise))
    (pcase state
      (`fulfilled (when (functionp fulfilled)
                    (funcall fulfilled promise-value)))
      (`rejected (when (funcionp rejected)
                   (funcall rejected promise-value)))
      (`pending
       (when (functionp fulfilled)
         (push fulfilled (nth 2 promise)))
       (when (functionp rejected)
         (push rejected (nth 3 promise))))))
  promise)


(defun p-all (promise &rest other-promises)
  "Creates a promise that is fulfilled when all promises are fulfilled
   and rejected when any of it is rejected"
  (lexical-let ((promises (append (list promise) other-promises)))
    (p-make-promise
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
                           (p-then promise
                                   (lambda (value)
                                     (funcall all-fulfilled value index))
                                   (lambda (reason)
                                     (funcall any-rejected reason)))))
                       promises))))))

(defun p-all-features (feature &rest other-features)
  "Creates a promise that is fulfilled when all features are loaded.

   Useful when loading packages with dependencies and the reason this library is done"
  (lexical-let ((features (append (list feature) other-features)))
    (apply #'p-all (mapcar
                    (lambda (feature)
                      (p-make-promise
                       (lambda (fulfiller _)
                         (eval-after-load feature
                           (progn
                             (funcall fulfiller feature))))))
                    features))))

(defmacro p-on-features (features &rest body )
  "A short macro to ease the use of using p-all-features"
  `(p-then
    (p-all-features ,@features)
    (lambda (loaded-features)
      ,@body
      )))


(provide 'promise)
;;; promise.el ends here
