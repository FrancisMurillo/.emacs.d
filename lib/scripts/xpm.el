;;; xpm.el --- XPM creation
;;
;; Filename: xpm.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Thu Sep 15 11:24:07 2016 (+0800)
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

(eval-when-compile (require 'cl))

(defun xpm-colon (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"10 26 2 1\",
\". c %s\",
\"  c %s\",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun xpm-colon-alt (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"10 26 2 1\",
\"  c %s\",
\". c %s\",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun xpm-arrow-left (color1 color2 &optional width height)
  "Return an XPM left arrow string representing."
  (lexical-let* ((forward-dots
       (lambda (dots width)
         (apply
          #'concat
          (mapcar
           (lambda (n)
             (if (<= n dots) "." " "))
           (number-sequence 1 width)))))
      (triangle-dots
       (lambda (width height)
         (lexical-let* ((factor (* 2.0 (float width) (/ (float height))))
             (half-height (/ height 2.0)))
           (string-join
            (mapcar
             (lambda (h)
               (lexical-let* ((normal-width
                    (if (<= h half-height)
                        h
                      (1+ (- height h))))
                   (dots (round (* normal-width factor)))
                   (inner-dots (funcall forward-dots dots width)))
                 (format "\"%s\"" inner-dots)))
             (number-sequence 1 height))
            ","))))
      (final-width (if width width 14))
      (final-height (if height height 26))
      (arrow-text
       (funcall triangle-dots final-width final-height)))
    (create-image
     (format "/* XPM */
static char * arrow_left[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             final-width
             final-height
             (if color1 color1 "None")
             (if color2 color2 "None")
             arrow-text)
     'xpm t :ascent 'center)))

(defun xpm-arrow-right (color1 color2 &optional width height)
  "Return an XPM right arrow string representing."
  (lexical-let* ((reverse-dots
       (lambda (dots width)
         (reverse
          (apply
           #'concat
           (mapcar
            (lambda (n)
              (if (<= n dots) "." " "))
            (number-sequence 1 width))))))
      (triangle-dots
       (lambda (width height)
         (lexical-let* ((factor (* 2.0 (float width) (/ (float height))))
             (half-height (/ height 2.0)))
           (string-join
            (mapcar
             (lambda (h)
               (lexical-let* ((normal-width
                    (if (<= h half-height)
                        h
                      (1+ (- height h))))
                   (dots (round (* normal-width factor)))
                   (inner-dots (funcall reverse-dots dots width)))
                 (format "\"%s\"" inner-dots)))
             (number-sequence 1 height))
            ","))))
      (final-width (if width width 14))
      (final-height (if height height 26))
      (arrow-text
       (funcall triangle-dots final-width final-height)))
    (create-image
     (format "/* XPM */
static char * arrow_left[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             final-width
             final-height
             (if color2 color2 "None")
             (if color1 color1 "None")
             arrow-text)
     'xpm t :ascent 'center)))

(defun xpm-curly-left (color1 color2 &optional width height)
  "Return an XPM left curly brace string representing."
  (lexical-let* ((forward-dots
       (lambda (dots width)
         (apply
          #'concat
          (mapcar
           (lambda (n)
             (if (<= n dots) "." " "))
           (number-sequence 1 width)))))
      (mapper
       (lambda (n) (* n n n)))
      (curly-dots
       (lambda (width height)
         (lexical-let* ((half-height (/ height 2.0)))
           (string-join
            (mapcar
             (lambda (h)
               (lexical-let* ((normal-width
                    (if (<= h half-height)
                        h
                      (- height h)))
                   (dots (round (* width (funcall mapper normal-width) (/ (funcall mapper half-height)))))
                   (inner-dots (funcall forward-dots dots width)))
                 (format "\"%s\"" inner-dots)))
             (number-sequence 1 height))
            ","))))
      (final-width (if width width 14))
      (final-height (if height height 26))
      (curly-text
       (funcall curly-dots final-width final-height)))
    (create-image
     (format "/* XPM */
static char * curly_left[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             final-width
             final-height
             (if color1 color1 "None")
             (if color2 color2 "None")
             curly-text)
     'xpm t :ascent 'center)))

(defun xpm-curly-right (color1 color2 &optional width height)
  "Return an XPM right curly brace string representing."
  (lexical-let* ((forward-dots
       (lambda (dots width)
         (apply
          #'concat
          (mapcar
           (lambda (n)
             (if (<= n dots) "." " "))
           (number-sequence 1 width)))))
      (mapper
       (lambda (n) (* n n n)))
      (curly-dots
       (lambda (width height)
         (lexical-let* ((half-height (/ height 2.0)))
           (string-join
            (mapcar
             (lambda (h)
               (lexical-let* ((normal-width
                    (if (<= h half-height)
                        h
                      (- height h)))
                   (dots (round (* width (- 1 (* (funcall mapper normal-width) (/ (funcall mapper half-height)))))))
                   (inner-dots (funcall forward-dots dots width)))
                 (format "\"%s\"" inner-dots)))
             (number-sequence 1 height))
            ","))))
      (final-width (if width width 14))
      (final-height (if height height 26))
      (curly-text
       (funcall curly-dots final-width final-height)))
    (create-image
     (format "/* XPM */
static char * curly_right[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             final-width
             final-height
             (if color1 color1 "None")
             (if color2 color2 "None")
             curly-text)
     'xpm t :ascent 'center)))

(defun xpm-slash-right (color1 color2 &optional width height)
  "Return an XPM right slash string representing."
  (lexical-let* ((forward-dots
       (lambda (dots width)
         (apply
          #'concat
          (mapcar
           (lambda (n)
             (if (<= n dots) "." " "))
           (number-sequence 1 width)))))
      (slash-dots
       (lambda (width height)
         (lexical-let* ((factor (/ (float width) (float height))))
           (string-join
            (mapcar
             (lambda (h)
               (lexical-let* ((reverse-width h)
                   (dots (round (* reverse-width factor)))
                   (inner-dots (reverse
                                (funcall forward-dots dots width))))
                 (format "\"%s\"" inner-dots)))
             (number-sequence 1 height))
            ","))))
      (final-width (if width width 14))
      (final-height (if height height 26))
      (slash-text
       (funcall slash-dots final-width final-height)))
    (create-image
     (format "/* XPM */
static char * slash_right[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             final-width
             final-height
             (if color2 color2 "None")
             (if color1 color1 "None")
             slash-text)
     'xpm t :ascent 'center)))


(defun xpm-curve-right (color1 color2)
  "Return an XPM right curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"        ....\",
\"      ......\",
\"      ......\",
\"      ......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"    ........\",
\"    ........\",
\"    ........\",
\"    ........\",
\"     .......\",
\"     .......\",
\"     .......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"        ....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun xpm-curve-left (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\"....        \",
\"......      \",
\"......      \",
\"......      \",
\".......     \",
\".......     \",
\".......     \",
\"........    \",
\"........    \",
\"........    \",
\"........    \",
\".......     \",
\".......     \",
\".......     \",
\"......      \",
\"......      \",
\"......      \",
\"....        \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun xpm-gradient-color-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'xpm-gradient-color-join
     (cl-mapcar
      (lambda (x y)
        (round (+ (* x alpha) (* y (- 1 alpha)))))
      c1 c2)))

(defun xpm-gradient-color-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun xpm-gradient (c1 c2)
  "Return an XPM gradient string representing."
  (let* ((backup-color
       (if (eq (get-buffer-window) mode-line-current-window)
           (face-attribute 'mode-line :background)
         (face-attribute 'mode-line-inactive :background)))
      (c1 (or c1 backup-color))
      (c2 (or c2 backup-color)))
    (create-image
     (format "/* XPM */
static char * gradient_left[] = {
/* columns rows colours chars-per-pixel */
\"12 26 12 1\",
\"a c %s\",
\"b c %s\",
\"c c %s\",
\"d c %s\",
\"e c %s\",
\"f c %s\",
\"g c %s\",
\"h c %s\",
\"i c %s\",
\"j c %s\",
\"k c %s\",
\"l c %s\",
/* pixels */
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\"};"
             c1
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.1)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.2)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.3)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.4)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.5)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.6)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.7)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.8)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 0.9)
             (xpm-gradient-color-blend (color-values c2) (color-values c1) 1.0)
             c2)
     'xpm t :ascent 'center)))

(defun xpm-slash-left (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"14 26 2 1\",
\". c %s\",
\"  c %s\",
\"............. \",
\"............. \",
\"............  \",
\"............  \",
\"...........   \",
\"...........   \",
\"..........    \",
\"..........    \",
\".........     \",
\".........     \",
\"........      \",
\"........      \",
\".......       \",
\".......       \",
\"......        \",
\"......        \",
\".....         \",
\".....         \",
\"....          \",
\"....          \",
\"...           \",
\"...           \",
\"..            \",
\"..            \",
\".             \",
\".             \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun xpm (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
        (idx  0))
      (apply 'concat
         (mapcar #'(lambda (dl)
                     (setq idx (+ idx 1))
                     (concat
                      "\""
                      (concat
                       (mapcar #'(lambda (d)
                                   (if (eq d 0)
                                       (string-to-char " ")
                                     (string-to-char ".")))
                               dl))
                      (if (eq idx len)
                          "\"};"
                        "\",\n")))
                 data))))
   'xpm t :ascent 'center))

(defun xpm-half (color1 color2)
  (xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun xpm-percent (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                0
              (round (* 17 (/ (float ws) (float pmax))))))
      (fe   (if (eq pmax we)
                17
              (round (* 17 (/ (float we) (float pmax))))))
      (o    nil)
      (i    0))
    (while (< i 18)
      (setq o (cons
            (if (and (<= fs i)
                   (<= i fe))
                (append (list 0) (make-list width 1) (list 0))
              (append (list 0) (make-list width 0) (list 0)))
            o))
      (setq i (+ i 1)))
    (xpm "percent" color1 color2 (reverse o))))


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun xpm-memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (xpm-memoize-wrap (symbol-function func))) func)
    (function (xpm-memoize-wrap func))))

(defun xpm-memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
      (val-sym (gensym))
      (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(xpm-memoize 'xpm-arrow-left)
(xpm-memoize 'xpm-arrow-right)
(xpm-memoize 'xpm-curve-left)
(xpm-memoize 'xpm-curve-right)
(xpm-memoize 'xpm-curly-left)
(xpm-memoize 'xpm-curly-right)
(xpm-memoize 'xpm-slash-left)
(xpm-memoize 'xpm-slash-right)
(xpm-memoize 'xpm-gradient)
(xpm-memoize 'xpm-colon)
(xpm-memoize 'xpm-colon-alt)


(provide 'xpm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xpm.el ends here
