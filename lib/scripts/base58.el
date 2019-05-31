;;; base58.el --- A base58 for Emacs Lisp  -*- lexical-binding: t; -*-
;;
;; Filename: base58.el
;; Description: A quick Emacs Lisp implementation of Base58
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Thu May 30 18:33:15 2019 (+0800)
;; Version: 0.0.1
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

(eval-when-compile
  (require 'calc)
  (require 'calc-ext)
  (require 'cl))

(require 'subr-x)

(defconst base58-bitcoin-alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "Bitcoin base58 alphabet")

(defconst base58-ipfs-alphabet base58-bitcoin-alphabet
  "IPFS base58 alphabet")

(defcustom base58-alphabet base58-bitcoin-alphabet
  "Alphabet to use for encoding and decoding.")

(defun base58-encode-string (string)
  "Encode `string' to Base58."
  (let* ((alphabet (split-string base58-alphabet "" t))
         (chars (split-string string "" t))
         (coefficients (mapcar #'string-to-char chars))
         (base10-value
          (cl-reduce
           (lambda (acc coefficient)
             (calc-eval "$ * 256 + $$"
                        nil
                        acc
                        coefficient))
           coefficients))
         (base58-coefficients
          (cl-loop with value = base10-value
                   and coefficients = (list)
                   until (string= value "0") do
                   (push (calc-eval "$ % 58" nil value) coefficients)
                   (setq value (calc-eval "$ \\ 58" nil value))
                   finally return (mapcar #'string-to-number coefficients)))
         (base58-chars (mapcar
                        (lambda (coefficient) (nth coefficient alphabet))
                        base58-coefficients)))
    (string-join base58-chars)))

(defun base58-decode-string (string)
  "Decode `string' from Base58."
  (let* ((alphabet (split-string base58-alphabet "" t))
         (chars (split-string string "" t))
         (coefficients
          (mapcar
           (lambda (char)
             (if-let ((value
                       (cl-position char alphabet :test #'string=)))
                 value
               (error "Not a Base58 string.")))
           chars))
         (base58-value
          (cl-reduce
           (lambda (acc coefficient)
             (calc-eval "$ * 58 + $$"
                        nil
                        acc
                        coefficient))
           coefficients))
         (base256-coefficients
          (cl-loop with value = base58-value
                   and coefficients = (list)
                   until (string= value "0") do
                   (push (calc-eval "$ % 256 " nil value) coefficients)
                   (setq value (calc-eval "$ \\ 256" nil value))
                   finally return (mapcar #'string-to-number coefficients)))
         (base256-chars (mapcar #'char-to-string base256-coefficients)))
    (string-join base256-chars)))


(provide 'base58)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; base58.el ends here
