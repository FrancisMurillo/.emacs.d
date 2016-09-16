;;; mode-line.el --- My custom mode line
;;
;; Filename: mode-line.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Wed Sep 14 14:49:55 2016 (+0800)
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

(require 'xpm)

(defvar mode-line-c1-primary "#383838")
(defvar mode-line-c2-primary "#666666")
(defvar mode-line-fg-primary "#bababa")
(defvar mode-line-c1-secondary "#383838")
(defvar mode-line-c2-secondary "#666666")
(defvar mode-line-fg-secondary "#bababa")

(defvar mode-line--default-height 130)

(defun mode-line-primary-window () (eq (get-buffer-window) mode-line-current-window))
(defun mode-line-c1 () (if (mode-line-primary-window) mode-line-c1-primary mode-line-c1-secondary))
(defun mode-line-c2 () (if (mode-line-primary-window) mode-line-c2-primary mode-line-c2-secondary))
(defun mode-line-fg () (if (mode-line-primary-window) mode-line-fg-primary mode-line-fg-secondary))

(defvar mode-theme-line-color-alist
  '((whiteboard     (:primary ("#bbbbbb" "#d7d7d7" "#2a2a2a") :secondary ("#bbbbbb" "#d7d7d7" "#2a2a2a")))
    (atom-one-dark  (:primary ("#3E4451" "#5C6370" "#ABB2BF") :secondary ("#3E4451" "#5C6370" "#ABB2BF")))
    (darktooth      (:primary ("#222222" "#222222" "#FDF3C3") :secondary ("#403935" "#403935" "#988975")))
    (niflheim       (:primary ("#222222" "#2a2a2a" "#bababa") :secondary ("#222222" "#2a2a2a" "#bababa")))
    (aurora         (:primary ("#455a64" "#2B3B40" "#CDD3D3") :secondary ("#232A2F" "#232A2F" "#556D79")))
    (forest-blue    (:primary ("#0e5994" "#203439" "#d3cbc4") :secondary ("#203439" "#203439" "#203439")))
    (eink           (:primary ("#DDDDD8" "#DDDDD8" "#383838") :secondary ("#DDDDD8" "#DDDDD8" "#DDDDD8")))
    (ujelly         (:primary ("#000000" "#000000" "#ffffff") :secondary ("#000000" "#000000" "#ffffff")))
    (spacemacs-dark (:primary ("#6c3163" "#292B2E" "#b2b2b2") :secondary ("#292B2E" "#292B2E" "#292B2E")))
    (solarized-dark (:primary ("#657b83" "#073642" "#073642") :secondary ("#002b36" "#002b36" "#586e75")))
    (gruvbox        (:primary ("#3c3836" "#282828" "#f4e8ba") :secondary ("#504945" "#282828" "#a89984")))
    (material       (:primary ("#1c1f26" "#1c1f26" "#ffffff") :secondary ("#1c1f26" "#1c1f26" "#a7adba")))
    (monokai        (:primary ("#363731" "#272822" "#E5DDB7") :secondary ("#272822" "#272822" "#75715E")))
    (darkokai       (:primary ("#ab7eff" "#242728" "#3D4345") :secondary ("#242728" "#242728" "#5D6365")))
    (suscolors      (:primary ("#5faf5f" "#262626" "#262626") :secondary ("#262626" "#262626" "#949494")))
    (wombat         (:primary ("#444444" "#343434" "#CCC9C0") :secondary ("#444444" "#343434" "#99968b")))))

(defun mode-line-update (&rest args)
  "Update the extra mode-line colours based on a mapping to theme."
  (interactive)
  (let* ((theme (car custom-enabled-themes))
         (primary-alist (plist-get (cadr (assoc theme mode-theme-line-color-alist)) :primary))
         (secondary-alist (plist-get (cadr (assoc theme mode-theme-line-color-alist)) :secondary)))
    (if (and primary-alist secondary-alist)
        (setq mode-line-c1-primary (car primary-alist)
              mode-line-c2-primary (cadr primary-alist)
              mode-line-fg-primary (caddr primary-alist)
              mode-line-c1-secondary (car secondary-alist)
              mode-line-c2-secondary (cadr secondary-alist)
              mode-line-fg-secondary (caddr secondary-alist))
      (setq mode-line-fg-primary "white"
            mode-line-fg-secondary "white"))))


(defun mode-line-set-style ()
  "Set the style of the mode-line separator"
  (interactive)
  (let* ((styles
          '(("arrow" xpm-arrow-left xpm-arrow-right)
            ("curve" xpm-curve-left xpm-curve-right)
            ("bolts" xpm-colon xpm-colon-alt)
            ("slash-/\\" xpm-slash-left xpm-slash-right)
            ("slash-//" xpm-slash-left xpm-slash-left)
            ("slash-\\/" xpm-slash-right xpm-slash-left)
            ("slash-\\\\" xpm-slash-right xpm-slash-right)
            ("gradient" xpm-gradient xpm-gradient)))
         (result (assoc (completing-read "Style: " styles) styles)))
    (defalias 'xpm-right (caddr result))
    (defalias 'xpm-left (cadr result))))

(defalias 'xpm-right 'xpm-arrow-right)
(defalias 'xpm-left  'xpm-arrow-left)

(defvar mode-line-minor-modes nil)
(defvar mode-line-arrow-shape 'arrow)
(defun mode-line-make-face
    (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "mode-line-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                              :foreground (mode-line-fg)
                              :background bg
                              :box nil))
        cface)
    nil))

(defun mode-line-make-left
    (string color1 &optional color2 localmap)
  (let ((plface (mode-line-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (xpm-left color1 color2)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq mode-line-arrow-shape 'arrow)
                                            (force-mode-line-update)))) ""))))



(defun mode-line-make-right
    (string color2 &optional color1 localmap)
  (let ((plface (mode-line-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (xpm-right color1 color2)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq mode-line-arrow-shape 'arrow)
                                            (force-mode-line-update))))
       "")
     (if arrown
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun mode-line-make-fill (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (mode-line-make-face color))
        (amount (- (window-total-width)
                   (+ (- 37 (* (face-attribute 'default :height) 0.11238))
                      (if (eq (get-buffer-window) mode-line-current-window)
                          (+ (length (-mode-line-get-weather "%(weather)"))
                             (length (-mode-line-get-weather "%(sunrise)  %(sunset)"))
                             (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode) 4 0))
                        0)
                      ;; (length (mode-line-flycheck-status))
                      ;; (length (format-mode-line "%l:%c"))
                      ;; (if mark-active (length (mode-line-region-info)) 0)
                      ;; (if (and (fboundp 'boop-format-results)
                      ;;          (eq (get-buffer-window) mode-line-current-window))
                      ;;     (+ 1 (length  (boop-format-results))) 0)
                      (length (-mode-line-get-temp))))))
    (propertize " " 'display `((space :align-to ,amount)) 'face plface)))


(defvar mode-line/render-center? t)
(defvar mode-line/colour-flycheck? nil)
(defvar mode-line/upgrades nil)



(defun mode-line-make-text
    (string color &optional fg localmap)
  (let ((plface (mode-line-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun mode-line-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (mode-line-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (mode-line-make-left   string color1 color2 localmap))
        ((eq side 'left)               (mode-line-make-left   string color1 color1 localmap))
        ((eq side 'right)              (mode-line-make-right  string color1 color1 localmap))
        ((eq side 'donttouch)          (mode-line-make-right  string color1 color1 localmap))
        (t                             (mode-line-make-text   string color1 localmap))))

(defmacro defmode-line (name string)
  "Macro to create a mode-line chunk."
  `(defun ,(intern (concat "mode-line-" (symbol-name name)))
       (side color1 &optional color2)
     (mode-line-make
      side
      (let ((result ,string))
        (cond ((listp result)
               (format-mode-line result))
              ((not (or (stringp result)
                        (null result)))
               (progn " ERR"))
              (t
               result)))
      color1 color2)))

(defmode-line arrow       "")

(defvar mode-line-buffer-size-suffix t)

(defun mode-line-process (&rest args)
  (let ((icon (all-the-icons-icon-for-buffer)))
    (concat
     (when (or (symbolp icon) ;; This implies it's the major mode
               mode-line-process)
       (propertize
        (format-mode-line " %m")
        'face `(:height 0.8 :foreground ,(mode-line-fg) :background ,(mode-line-c1))
        'display '(raise 0.0)))
     (when mode-line-process
       (propertize (format-mode-line mode-line-process)
                   'face `(:height 0.8 :foreground ,(mode-line-fg) :background ,(mode-line-c1)))))))



(defun mode-line-project-id (&rest args)
  (lexical-let ((default-props
                  (list 'face (list :height 0.8))).)
    (if (and (fboundp 'projectile-project-name)
             (projectile-project-name))
        (format
         " | %s |"
         (propertize
          (format "%s" (projectile-project-name))
          'face '(:height 0.8)
          'help-echo "Switch Project"
          'mouse-face '(:box 1)
          'local-map (make-mode-line-mouse-map
                      'mouse-1 (lambda () (interactive) (projectile-switch-project)))))
      (propertize " | × |" 'face '(:height 0.8)))))

(defun mode-line-buffer-id (&rest args)
  (if (and (fboundp 'projectile-project-root))
      (let* ((buf (or (buffer-file-name) (buffer-name)))
             (proj (ignore-errors (projectile-project-root)) )
             (name (if (buffer-file-name)
                       (or (cadr (split-string buf proj))
                           (format-mode-line "%b"))
                     (format-mode-line "%b"))))
        (propertize (format "  %s" name)
                    'face `(:height 0.8)
                    'help-echo (format "Major-mode: `%s`" major-mode)))
    (propertize (format-mode-line "  %b") 'face '(:height 0.8))))

(defun mode-line-flycheck-status (&rest args)
  (when (fboundp 'flycheck-status-emoji-mode-line-text)
    (let* ((text (cadr (flycheck-status-emoji-mode-line-text)))
           (fg (cond
                ((not mode-line/colour-flycheck?) (mode-line-fg))
                ((string-match "Disabled" text) (mode-line-fg))
                ((string-match "Running" text) (mode-line-fg))
                ((string-match "⚠" text) (face-attribute 'warning :foreground))
                ((string-match "✖" text) (face-attribute 'error :foreground))
                (t (face-attribute 'success :foreground)))))
      (concat
       (when (and
              vc-mode
              (eq (get-buffer-window) mode-line-current-window)
              mode-line/render-center?)
         (propertize " ·" 'face `(:foreground ,(mode-line-fg) :background ,(mode-line-c2))))
       (propertize (format " %s" text)
                   'face `(:height 0.9 :foreground ,fg :background ,(mode-line-c2))
                   'help-echo "Show Flycheck Errors"
                   ;; 'display '(raise 0.1)
                   'mouse-face '(:box 1)
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))))



(defmode-line gap "")

(defun mode-line-window-number (&rest args)
  (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
              'face `(:height ,(/ (* 0.90 mode-line--default-height) 100.0))
              'display '(raise 0.0)))

(defvar mode-line-current-window nil)
(defun update-current-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq mode-line-current-window (selected-window))))
(add-function :before pre-redisplay-function 'update-current-window)

(defun mode-line-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(defun mode-line-mode-default ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (when (symbolp icon) ;; This implies it's the major mode
      (propertize
       (format-mode-line " %m")
       'face `(:height 0.8 :foreground ,(mode-line-fg) :background ,(mode-line-c1))
       'display '(raise 0.1)))))



(defun mode-line-modified ()
  "Indicates if the buffer is modified."
  (let* ((config-alist
       '(("*"
          all-the-icons-faicon-family
          all-the-icons-faicon
          "chain-broken"
          :height 1.2
          :v-adjust -0.0)
         ("-"
          all-the-icons-faicon-family
          all-the-icons-faicon
          "link"
          :height 1.2
          :v-adjust -0.0)
         ("%"
          all-the-icons-octicon-family
          all-the-icons-octicon
          "lock"
          :height 1.2
          :v-adjust -0.0)))
      (result (cdr (assoc (format-mode-line "%*") config-alist)))
      (icon-font-function (car result))
      (icon-function (cadr result))
      (icon-args (cddr result)))
    (propertize
     (apply icon-function icon-args)
     'face
     (list
      :family (funcall icon-font-function)))))

(provide 'mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-line.el ends here
