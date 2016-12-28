;;; text-candy.el ---
;;
;; Filename: text-candy.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Sat Oct 15 08:19:55 2016 (+0800)
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

(defcustom text-candy-words
  (list
   (cons "freenode"
      (concat  ;; freenode
       (propertize
        "free" 'face '(:foreground "#e6e6e6"))
       (propertize
        "node" 'face '(:foreground "#4beb4a"))))

   (cons "emacs"
      (propertize
       "emacs"
       'face
       '(:foreground "#ffffff" :background "#7e5ab6"
                     :box (:line-width 2 :color "#592982") :weight bold :height 0.9)))
   (cons "google"
      (concat ;; google
       (propertize "g" 'face '(:foreground "#4285f4"))
       (propertize "o" 'face '(:foreground "#ea4335"))
       (propertize "o" 'face '(:foreground "#fbbc05"))
       (propertize "g" 'face '(:foreground "#4285f4"))
       (propertize "l" 'face '(:foreground "#34a853"))
       (propertize "e" 'face '(:foreground "#ea4335"))))

   (cons "clojure"
      (concat  ;; clojure
       (propertize
        "clo" 'face '(:foreground "#63b132" :box (:line-weight 2 :color "ffffff")))
       (propertize
        "j" 'face '(:foreground "#ffffff"))
       (propertize
        "ure" 'face '(:foreground "#5881d8")))))
  "An alist of words with their `cdr' as its colored form."
  :type '(alist :key-type string :value-type string)
  :group 'fn)

(defun text-candy-add-candy-word (candied-word)
  "Add CANDIED-WORD to the list."
  (add-to-list
   'text-candy-words
   (cons
    (substring-no-properties candied-word)
    candied-word)))

(defun text-candy-candied-word (word)
  "Get candied WORD, if none exist return the WORD itself."
  (or (cdr (assoc word text-candy-words))
     word))

(provide 'text-candy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-candy.el ends here
