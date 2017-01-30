;;; projin.el --- Write projectile ignore files with a DSL  -*- lexical-binding: t; -*-
;;
;; Filename: projin.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Wed Dec 28 21:36:00 2016 (+0800)
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
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

(defconst projin-compiled-file-name ".projectile"
  "Projin compiled file name.")

(defconst projin-dsl-prefix "projin--dsl-"
  "Prefix to detect what rule to use.")

(defconst projin-block-prefix "projin--block-"
  "Prefix to detect what block to use.")

(defconst projin-comment-beginning "#"
  "The start of a comment line.")


;;;###autoload
(defun projin-compile (dsl)
  "Compile DSL to text."
  (projin--compiler dsl (list)))


;; Rules
(defun projin--compiler (dsl &optional env)
  "Main compiler DSL with an ENV."
  (pcase-let ((`(,rule . ,_) dsl))
    (lexical-let* ((rule-name (symbol-name rule))
                   (rule-handler
                    (intern-soft
                     (format "%s%s"
                             projin-dsl-prefix
                             rule-name))))
      (if (null rule-handler)
          (error "No rule to handle %s at dsl: %s" rule-name dsl)
        (funcall rule-handler dsl env)))))


(defun projin--path-separator ()
  "Return path separator."
  (lexical-let ((path (expand-file-name "_")))
    (substring-no-properties
     path
     (- (length path) 2)
     (- (length path) 1))))

;; Context Rule
(defun projin--dsl-context (dsl env)
  "DSL and ENV for context."
  (pcase-let ((`(,(let (or 'context 'include 'override 'root 'path) '_) . ,subdsls) dsl))
    (string-join
     (mapcar
      (lambda (subdsl)
        (funcall #'projin--compiler subdsl env))
      subdsls)
     "\n")))

(defun projin--dsl-root (dsl env)
  "DSL and ENV for root."
  (pcase-let ((`(root . ,_) dsl))
    (lexical-let ((new-env (append (list (cons :parent "/")) env)))
      (projin--dsl-context dsl new-env))))

(defun projin--dsl-comment (dsl _)
  "DSL and ENV for comment."
  (pcase-let ((`(comment ,comment) dsl))
    (format "%s %s" projin-comment-beginning comment)))

(defun projin--dsl-newline (dsl _)
  "DSL and ENV for newline."
  (pcase-let ((`(newline) dsl))
    (format "")))

(defun projin--dsl-file (dsl env)
  "DSL and ENV for file."
  (pcase-let ((`(file ,file) dsl))
    (lexical-let* ((parent (cdr (assoc :parent env)))
                   (include (if (cdr (assoc :include env)) "+" "-"))
                   (override (if (cdr (assoc :override env)) "!" nil)))
      (concat (or override include) parent file))))

(defalias 'projin--dsl-dir 'projin--dsl-file
  "DSL and ENV for dir. The same as `-file', but with a contextual nuance.'")

(defun projin--dsl-include (dsl env)
  "DSL and ENV for include."
  (pcase-let ((`(include . ,_) dsl))
    (lexical-let ((new-env (append (list (cons :include t)) env)))
      (projin--dsl-context dsl new-env))))

(defun projin--dsl-override (dsl env)
  "DSL and ENV for override."
  (pcase-let ((`(override . ,_) dsl))
    (lexical-let ((new-env (append (list (cons :override t)) env)))
      (projin--dsl-context dsl new-env))))

(defun projin--dsl-path (dsl env)
  "DSL and ENV for path."
  (pcase-let ((`(path ,path . ,subdsls) dsl))
    (message "%s" subdsls)
    (lexical-let* ((parent (cdr (assoc :parent env)))
                   (new-env (append
                             (list (cons :parent (concat parent path (projin--path-separator))))
                             env)))
      (projin--dsl-context `(context ,@subdsls) new-env))))

(defun projin--dsl-block (dsl env)
  "DSL and ENV for block."
  (pcase-let ((`(block ,block-symbol) dsl))
    (lexical-let* ((block-name (symbol-name block-symbol))
                   (block-value (intern-soft
                                 (format "%s%s"
                                         projin-block-prefix
                                         block-name))))
      (if (null block-value)
          (error "No rule to block %s at dsl: %s" block-name dsl)
        (projin--compiler (symbol-value block-value) env)))))

(defun projin--dsl-defblock (dsl _)
  "DSL and ENV for defblock."
  (pcase-let ((`(defblock ,block-symbol . ,block-def) dsl))
    (lexical-let* ((block-name (symbol-name block-symbol))
                   (block-def-name (intern
                                    (format "%s%s"
                                            projin-block-prefix
                                            block-name))))
      (makunbound block-def-name)
      (eval `(defvar ,block-def-name '(context ,@block-def)
               ,(format "Block definition for %s" block-name)))
      block-def-name)))


(defun projin--dsl-delimited (dsl env)
  "DSL and ENV for delimited."
  (pcase-let ((`(delimited  . ,subdsls) dsl))
    (lexical-let ((delimited-dsls
                   (cdr
                    (apply #'append
                           (mapcar
                            (lambda (dsl)
                              (list '(newline) dsl))
                            subdsls)))))
      (projin--compiler
       `(context
         ,@delimited-dsls)
       env))))


;;;###autoload
(defun projin-write-to-project (dsl project)
  "Write compiled DSL to PROJECT."
  (lexical-let* ((compiled-file (expand-file-name projin-compiled-file-name project))
                 (compiled-text (projin-compile dsl)))
    (with-temp-file compiled-file
      (insert compiled-text))
    (message "%s of %s updated" projin-compiled-file-name project)))


;; Preset Blocks
(projin--dsl-defblock
 `(defblock java
    (comment "Block for java")
    (file "*.class")
    (file "*.jar")

    (root
     (file "target")))
 (list))

(projin--dsl-defblock
 `(defblock emacs
    (comment "Block for emacs")
    (file ".#*")
    (file "*.elc"))
 (list))

(projin--dsl-defblock
 `(defblock lein
    (comment "Block for lein")
    (root
     (dir ".cljs*")
     (dir "target")))
 (list))


(provide 'projin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projin.el ends here
