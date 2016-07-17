;;; package: --- Summary

;;; Commentary:

;;; Code:

(require 'promise)

(ert-deftest promise-then-fulfill-immediately ()
  (lexical-let* ((value 42)
                 (operation (p-make-promise
                             (lambda (res rej)
                               (funcall res value)))))
    (should (eq 'fulfilled (p-state operation)))
    (p-then (lambda (the-value)
              (should (eq value the-value)))
            nil)))


(ert-deftest promise-then-reject-immediately ()
  (lexical-let* ((reason "Not enough mana")
                 (operation (p-make-promise
                             (lambda (res rej)
                               (funcall rej reason)))))
    (should (eq 'rejected (p-state operation)))
    (p-then nil
            (lambda (the-reason)
              (should (eq reason the-reason))))))


(ert-deftest promise-then-immutable-state ()
  (lexical-let* ((final-value 'up)
                 (operation (p-make-promise
                             (lambda (res rej)
                               (funcall res final-value)
                               (funcall rej 'down)
                               (funcall res 'left)))))
    (should (eq 'fulfilled (p-state operation)))
    (p-then (lambda (the-value)
              (should (eq final-value the-value)))
            (lambda (the-reason)
              (should-not the-reason)))))


(ert-deftest promise-then-delayed ()
  (lexical-let* ((delayed-value 1.618)
                 (resolver nil)
                 (operation (p-make-promise
                             (lambda (res rej)
                               (setq resolver res)))))
    (should (eq 'pending (p-state operation)))
    (funcall resolver delayed-value)
    (should (eq 'fulfilled (p-state operation)))
    (p-then (lambda (the-value)
              (should (eq delayed-value the-value))))))


(ert-deftest promise-all-fulfilled ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'right)))))
    (should (eq 'fulfilled (p-state left-operation)))
    (should (eq 'fulfilled (p-state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (pcase-let ((`(,left ,right) values))
                (should (eq 'left left))
                (should (eq 'right right))))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-rejected ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall rej 'right)))))
    (should (eq 'fulfilled (p-state left-operation)))
    (should (eq 'rejected (p-state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should (eq 'right reason))))))


(ert-deftest promise-all-forever ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     nil))))
    (should (eq 'fulfilled (p-state left-operation)))
    (should (eq 'pending (p-state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-features ()
  (p-then
   (p-all-features 'ert 'promise)
   (lambda (_)
     (should t))))
