;;; magin.el --- Write git ignore files with a DSL  -*- lexical-binding: t; -*-
;;
;; Filename: magin.el
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

(defconst magin-compiled-file-name ".gitignore"
  "Magin compiled file name.")

(defconst magin-dsl-prefix "magin--dsl-"
  "Prefix to detect what rule to use.")

(defconst magin-block-prefix "magin--block-"
  "Prefix to detect what block to use.")

(defconst magin-comment-beginning "#"
  "The start of a comment line.")


;;;###autoload
(defun magin-compile (dsl)
  "Compile DSL to text."
  (magin--compiler dsl (list)))


;; Rules
(defun magin--compiler (dsl &optional env)
  "Main compiler DSL with an ENV."
  (pcase-let ((`(,rule . ,_) dsl))
    (lexical-let* ((rule-name (symbol-name rule))
        (rule-handler
         (intern-soft
          (format "%s%s"
                  magin-dsl-prefix
                  rule-name))))
      (if (null rule-handler)
          (error "No rule to handle %s at dsl: %s" rule-name dsl)
        (funcall rule-handler dsl env)))))


(defun magin--path-separator ()
  "Return path separator."
  (lexical-let ((path (expand-file-name "_")))
    (substring-no-properties
     path
     (- (length path) 2)
     (- (length path) 1))))

;; Context Rule
;; Children: -root, -file
(defun magin--dsl-context (dsl env)
  "DSL and ENV for context."
  (pcase-let ((`(,(let (or 'context 'include 'root 'path) '_) . ,subdsls) dsl))
    (string-join
     (mapcar
      (lambda (subdsl)
        (funcall #'magin--compiler subdsl env))
      subdsls)
     "\n")))

(defun magin--dsl-root (dsl env)
  "DSL and ENV for root."
  (pcase-let ((`(root . ,_) dsl))
    (lexical-let ((new-env (append (list (cons :parent "/")) env)))
      (magin--dsl-context dsl new-env))))

(defun magin--dsl-comment (dsl _)
  "DSL and ENV for comment."
  (pcase-let ((`(comment ,comment) dsl))
    (format "%s %s" magin-comment-beginning comment)))

(defun magin--dsl-newline (dsl _)
  "DSL and ENV for newline."
  (pcase-let ((`(newline) dsl))
    (format "")))

(defun magin--dsl-file (dsl env)
  "DSL and ENV for file."
  (pcase-let ((`(file ,file) dsl))
    (lexical-let* ((parent (cdr (assoc :parent env)))
                   (include (if (cdr (assoc :include env))"!" nil)))
      (concat include parent file))))

(defalias 'magin--dsl-dir 'magin--dsl-file
  "DSL and ENV for dir. The same as `-file', but with a contextual nuance.'")

(defun magin--dsl-include (dsl env)
  "DSL and ENV for include."
  (pcase-let ((`(include . ,_) dsl))
    (lexical-let ((new-env (append (list (cons :include t)) env)))
      (magin--dsl-context dsl new-env))))

(defun magin--dsl-path (dsl env)
  "DSL and ENV for path."
  (pcase-let ((`(path ,path . ,subdsls) dsl))
    (message "%s" subdsls)
    (lexical-let* ((parent (cdr (assoc :parent env)))
        (new-env (append
                  (list (cons :parent (concat parent path (magin--path-separator))))
                  env)))
      (magin--dsl-context `(context ,@subdsls) new-env))))

(defun magin--dsl-block (dsl env)
  "DSL and ENV for block."
  (pcase-let ((`(block ,block-symbol) dsl))
    (lexical-let* ((block-name (symbol-name block-symbol))
        (block-value (intern-soft
                      (format "%s%s"
                              magin-block-prefix
                              block-name))))
      (if (null block-value)
          (error "No rule to block %s at dsl: %s" block-name dsl)
        (magin--compiler (symbol-value block-value) env)))))

(defun magin--dsl-defblock (dsl _)
  "DSL and ENV for defblock."
  (pcase-let ((`(defblock ,block-symbol . ,block-def) dsl))
    (lexical-let* ((block-name (symbol-name block-symbol))
        (block-def-name (intern
                         (format "%s%s"
                                 magin-block-prefix
                                 block-name))))
      (makunbound block-def-name)
      (eval `(defvar ,block-def-name '(context ,@block-def)
               ,(format "Block definition for %s" block-name)))
      block-def-name)))


(defun magin--dsl-delimited (dsl env)
  "DSL and ENV for delimited."
  (pcase-let ((`(delimited  . ,subdsls) dsl))
    (lexical-let ((delimited-dsls
         (cdr
          (apply #'append
             (mapcar
              (lambda (dsl)
                (list '(newline) dsl))
              subdsls)))))
      (magin--compiler
       `(context
         ,@delimited-dsls)
       env))))


;;;###autoload
(defun magin-write-to-project (dsl project)
  "Write compiled DSL to PROJECT."
  (lexical-let* ((compiled-file (expand-file-name magin-compiled-file-name project))
      (compiled-text (magin-compile dsl)))
    (with-temp-file compiled-file
      (insert compiled-text))
    (message "%s of %s updated" magin-compiled-file-name project)))


;; Preset Blocks
(magin--dsl-defblock
 `(defblock gtags
    (comment "Block for gtags")
    (file "GPATH")
    (file "GTAGS")
    (file "GRTAGS"))
 (list))

(magin--dsl-defblock
 `(defblock emacs
    (comment "Block for emacs")
    (file "*.elc")
    (file ".#*"))
 (list))


(provide 'magin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magin.el ends here
