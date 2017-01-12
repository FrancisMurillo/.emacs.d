;;; org-jekyll-blogger.el --- A blogging tool for jekyll and org   -*- lexical-binding: t; -*-
;;
;; Filename: org-jekyll-blogger.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Thu Dec 22 17:37:54 2016 (+0800)
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords: convenience
;; Compatibility: 24.4
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

;; Imports
(eval-when-compile
  (require 'cl))

(require 'subr-x)

(require 'org)
(require 'ox-publish)


;; Header
(eval-when-compile
  (defvar org-publish-project-alist))


;; Options
(defcustom
  org-jekyll-blogger-default-post-headers
  (list
   :H 2
   :num nil
   :tags nil
   :timestamp t)
  "Default post headers."
  :type '(plist)
  :group 'fn)

(defcustom
  org-jekyll-blogger-default-post-options
  (list
   :tagline "Check out this new post")
  "Default post (front matter) options."
  :type '(plist)
  :group 'fn)


(defcustom
  org-jekyll-blogger-find-file-hook
  nil
  "Hook when a org-jekyll-blogger file is opened."
  :type 'hook
  :group 'fn)

(defcustom
  org-jekyll-blogger-dir-change-hook
  nil
  "Hook when a org-jekyll-blogger directory is changed."
  :type 'hook
  :group 'fn)


;; Constants
(defconst org-jekyll-blogger--post-ext ".org"
  "Name of the post extension.")

(defconst org-jekyll-blogger--posts-dir-name "_posts"
  "Name of the posts directory of jekyll.")

(defconst org-jekyll-blogger--drafts-dir-name "_drafts"
  "Name of the drafts directory of jekyll.")

(defconst org-jekyll-blogger--data-dir-name "_data"
  "Name of the drafts directory of jekyll.")

(defconst org-jekyll-blogger--images-dir-name "images"
  "Name of the images directory of jekyll.")


;; Variables
(defvar org-jekyll-blogger--projects (make-hash-table :test 'equal)
  "Current (org) jekyll projects.")

(defvar org-jekyll-blogger--blogs (make-hash-table :test 'equal)
  "Current (org) jekyll blogs.")

(defvar org-jekyll-blogger--change-timer nil
  "Timer for checking if directory is modified.")

(defvar org-jekyll-blogger--change-table (make-hash-table :test 'equal)
  "Table for changing time.")


;; Plist
(defun org-jekyll-blogger--plist-keys (plist)
  "Get the keys of PLIST."
  (cl-reduce
   (lambda (item acc)
     (if (and (symbolp item)
              (equal
               (substring-no-properties (symbol-name item) 0 1)
               ":"))
         (cons item acc)
       acc))
   plist
   :initial-value (list)
   :from-end t))

(defun org-jekyll-blogger--plist-to-alist (plist)
  "Convert a PLIST into an alist."
  (mapcar
   (lambda (key)
     (cons key
           (plist-get plist key)))
   (org-jekyll-blogger--plist-keys plist)))

(defun org-jekyll-blogger--plist-merge (&rest plists)
  "Merge the PLISTS into a new one."
  (cl-reduce
   (lambda (acc plist)
     (cl-reduce
      (lambda (acc pair)
        (pcase-let ((`(,key . ,value) pair))
          (plist-put acc key value)))
      (org-jekyll-blogger--plist-to-alist plist)
      :initial-value acc
      :from-end nil))
   plists
   :initial-value (list)
   :from-end nil))


;; Alist
(defun org-jekyll-blogger--alist-p (values)
  "Check if VALUES is an alist."
  (cl-reduce
   (lambda (acc value)
     (if acc
         (and acc (consp value))
       nil))
   values
   :initial-value t
   :from-end nil))


;; Headers
(defun org-jekyll-blogger--format-post-headers (headers)
  "Turn HEADERS into a compatible (org) options."
  (format
   "#+OPTIONS: %s"
   (string-join
    (mapcar
     (lambda (pair)
       (pcase-let ((`(,key . ,value) pair))
         (format
          "%s:%s"
          (substring-no-properties
           (symbol-name key)
           1)
          value)))
     (org-jekyll-blogger--plist-to-alist headers))
    " ")))

(defun org-jekyll-blogger--format-post-options (options)
  "Turn OPTIONS into a (front matter) options."
  (format
   "#+BEGIN_EXPORT html\n%s\n%s\n%s\n#+END_EXPORT"
   "---"
   (string-join
    (mapcar
     (lambda (pair)
       (pcase-let ((`(,key . ,value) pair))
         (format
          "%s: %s"
          (substring-no-properties
           (symbol-name key)
           1)
          (if (listp value)
              (format
               "\n%s"
               (string-join
                (mapcar
                 (lambda (item)
                   (format "- %s" item))
                 value)
                "\n"))
            value))))
     (org-jekyll-blogger--plist-to-alist options))
    "\n")
   "---"))

(defun org-jekyll-blogger--format-post-preamble (post)
  "Format the preamble for the POST, header and options."
  (string-join
   (list
    (org-jekyll-blogger--format-post-headers
     (plist-get post :headers))
    (org-jekyll-blogger--format-post-options
     (plist-get post :options)))
   "\n"))


;; Lists
(defun org-jekyll-blogger-projects ()
  "Get projects."
  (hash-table-values org-jekyll-blogger--projects))

(defun org-jekyll-blogger-blogs ()
  "Get blogs."
  (hash-table-values org-jekyll-blogger--blogs))


;; Project
(defun* org-jekyll-blogger-define-project (project-name &key project-root publish-root post-headers post-options)
  "Define a jekyll project specialed with an org project mindset."
  (lexical-let ((project
                 (list
                  :project-name project-name
                  :project-root (expand-file-name project-root)
                  :publish-root (expand-file-name publish-root)

                  :post-headers (or post-headers org-jekyll-blogger-default-post-headers)
                  :post-options (or post-options org-jekyll-blogger-default-post-options))))
    (org-jekyll-blogger--setup-project-structure project)
    (org-jekyll-blogger--update-tables-by-project project)
    (org-jekyll-blogger--make-org-project project)

    project))

(defun org-jekyll-blogger--project-data-dir (project)
  "Compute a PROJECT's data directory from `org-jekyll-blogger--data-dir-name'."
  (expand-file-name
   org-jekyll-blogger--data-dir-name
   (plist-get project :publish-root)))

(defun org-jekyll-blogger--setup-project-structure (project)
  "Setup PROJECT structure."
  (lexical-let ((project-root
                 (plist-get project :project-root))
                (publish-root
                 (plist-get project :publish-root)))
    (make-directory project-root t)
    (make-directory publish-root t)

    (make-directory (org-jekyll-blogger--project-data-dir project) t)))


(defun org-jekyll-blogger--update-tables-by-project (project)
  "Update `org-jekyll-blogger--blogs' and `org-jekyll-blogger--projects' by PROJECT."
  (lexical-let ((project-name
                 (plist-get project :project-name)))
    ;; Update project list
    (puthash
     project-name
     project
     org-jekyll-blogger--projects)

    ;; Delete blogs linked to the project
    (-each
        (hash-table-values org-jekyll-blogger--blogs)
      (lambda (blog)
        (when (string=
               (plist-get blog :project-name)
               project-name)
          (org-jekyll-blogger--remove-blog blog))))))

(defun org-jekyll-blogger--make-org-project (project)
  "Make `org-publish' recognize PROJECT."
  (lexical-let ((project-name (org-jekyll-blogger--project-command project)))
    (setq org-publish-project-alist
          (remove
           (assoc project-name org-publish-project-alist)
           org-publish-project-alist))

    (add-to-list
     'org-publish-project-alist
     (list project-name
           :components (list)))))


;; Blog
(defun* org-jekyll-blogger-define-blog (blog-name &key project blog-root blog-publish-root post-headers post-options)
  "Define a jekyll blog."
  (lexical-let* ((project-root (plist-get project :project-root))
                 (project-name (plist-get project :project-name))
                 (publish-root (plist-get project :publish-root))

                 (new-blog)
                 (blog
                  (list
                   :blog-name (or blog-name "PRIMARY")
                   :blog-root
                   (if blog-root
                       blog-root
                     (expand-file-name
                      (or blog-name ".")
                      project-root))
                   :blog-publish-root
                   (if blog-publish-root
                       blog-publish-root
                     (expand-file-name
                      (or blog-name ".")
                      publish-root))

                   :project-name project-name

                   :post-headers
                   (org-jekyll-blogger--plist-merge
                    (plist-get project :post-headers)
                    post-headers)

                   :post-options
                   (org-jekyll-blogger--plist-merge
                    (plist-get project :post-options)
                    post-options))))
    (org-jekyll-blogger--setup-blog-structure blog)

    (org-jekyll-blogger--update-tables-by-blog blog)
    (org-jekyll-blogger--make-org-blog blog)

    blog))

(defun org-jekyll-blogger--update-tables-by-blog (blog)
  "Update `org-jekyll-blogger--blogs' and `org-jekyll-blogger--projects' by BLOG."
  (lexical-let ((blog-name
                 (plist-get blog :blog-name)))
    ;; Update project list
    (puthash
     (org-jekyll-blogger--blog-key
      (plist-get blog :project-name)
      blog-name)
     blog
     org-jekyll-blogger--blogs)))

(defun org-jekyll-blogger--setup-blog-structure (blog)
  "Setup BLOG structure."
  (lexical-let ((blog-root
       (plist-get blog :blog-root))
      (blog-publish-root
       (plist-get blog :blog-publish-root)))
    (make-directory blog-root t)
    (make-directory blog-publish-root t)

    (make-directory (org-jekyll-blogger--blog-post-dir blog) t)
    (make-directory (org-jekyll-blogger--blog-draft-dir blog) t)
    (make-directory (org-jekyll-blogger--blog-data-dir blog) t)
    (make-directory (org-jekyll-blogger--blog-image-dir blog) t)nil))

(defun org-jekyll-blogger--project-command (project)
  "Compute a PROJECT's publish command."
  (plist-get project :project-name))

(defun org-jekyll-blogger--remove-blog (blog)
  "Remove BLOG from table and its associations on `org-publish'."
  (remhash (plist-get blog :blog-name) org-jekyll-blogger--blogs)

  (lexical-let* ((command-name (org-jekyll-blogger--blog-command blog))
      (publish-project (assoc command-name org-publish-project-alist)))
    (when publish-project
      (setq org-publish-project-alist
         (remove publish-project org-publish-project-alist))
      (lexical-let ((support-publish-commands
           (plist-get (cdr publish-project) :components)))
        (mapc
         (lambda (support-command-name)
           (setq org-publish-project-alist
              (remove
               (assoc support-command-name org-publish-project-alist)
               org-publish-project-alist)))
         support-publish-commands)))

    (lexical-let* ((project-name (plist-get blog :project-name))
        (project-command (assoc project-name org-publish-project-alist))
        (project-blogs (plist-get (cdr project-command) :components))
        (updated-project-blogs (remove command-name project-blogs)))
      (setq org-publish-project-alist
            (remove project-command org-publish-project-alist))
      (add-to-list
       'org-publish-project-alist
       (list project-name
             :components updated-project-blogs)))))


(defun org-jekyll-blogger--blog-command (blog)
  "Compute a BLOG's publish command."
  (format "%s->%s"
          (plist-get blog :project-name)
          (plist-get blog :blog-name)))


(defun org-jekyll-blogger--blog-key (project-name blog-name)
  "Compute a blog key by PROJECT-NAME and BLOG-NAME"
  (format "%s->%s" project-name blog-name))

(defun org-jekyll-blogger--blog-post-dir (blog)
  "Compute a BLOG's posts directory from `org-jekyll-blogger--posts-dir-name'"
  (expand-file-name
   org-jekyll-blogger--posts-dir-name
   (plist-get blog :blog-root)))

(defun org-jekyll-blogger--blog-data-dir (blog)
  "Compute a BLOG's posts directory from `org-jekyll-blogger--data-dir-name'"
  (expand-file-name
   org-jekyll-blogger--data-dir-name
   (plist-get blog :blog-root)))

(defun org-jekyll-blogger--blog-draft-dir (blog)
  "Compute a BLOG's drafts directory from `org-jekyll-blogger--drafts-dir-name'"
  (expand-file-name
   org-jekyll-blogger--drafts-dir-name
   (plist-get blog :blog-root)))

(defun org-jekyll-blogger--blog-image-dir (blog)
  "Compute a BLOG's images directory from `org-jekyll-blogger--images-dir-name'"
  (expand-file-name
   org-jekyll-blogger--images-dir-name
   (plist-get blog :blog-root)))


(defun org-jekyll-blogger--blog-static-command (blog)
  "Give BLOG static command."
  (format "%s--static" (org-jekyll-blogger--blog-command blog)))

(defun org-jekyll-blogger--blog-data-command (blog)
  "Give BLOG data command."
  (format "%s--data" (org-jekyll-blogger--blog-command blog)))

(defun org-jekyll-blogger--blog-content-command (blog)
  "Give BLOG content command."
  (format "%s--content" (org-jekyll-blogger--blog-command blog)))


(defun org-jekyll-blogger--make-org-blog (blog)
  "Make `org-publish' recognize BLOG."
  (lexical-let* ((command-name
       (org-jekyll-blogger--blog-command blog))
      (blog-root
       (plist-get blog :blog-root))
      (blog-publish-root
       (plist-get blog :blog-publish-root))
      (project
       (gethash
        (plist-get blog :project-name)
        org-jekyll-blogger--projects))
      (publish-content-command
       (org-jekyll-blogger--blog-content-command blog))
      (publish-static-command
       (org-jekyll-blogger--blog-static-command blog))
      (publish-data-command
       (org-jekyll-blogger--blog-data-command blog))
      (publish-command command-name))
    (add-to-list
     'org-publish-project-alist
     (list publish-content-command
        :base-directory blog-root
        :base-extension "org"
        :publishing-directory blog-publish-root
        :exclude org-jekyll-blogger--data-dir-name
        :recursive t
        :publishing-function 'org-html-publish-to-html
        :html-extension "html"
        :headline-levels 4
        :body-only t
        :with-toc nil))

    (add-to-list
     'org-publish-project-alist
     (list publish-static-command
        :base-directory blog-root
        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
        :publishing-directory blog-publish-root
        :recursive t
        :publishing-function 'org-publish-attachment))

    (add-to-list
     'org-publish-project-alist
     (list publish-data-command
        :base-directory (org-jekyll-blogger--blog-data-dir blog)
        :base-extension "yaml"
        :publishing-directory (org-jekyll-blogger--project-data-dir project)
        :publishing-function 'org-publish-attachment))

    (add-to-list
     'org-publish-project-alist
     (list publish-command
        :components (list publish-content-command publish-static-command publish-data-command)))

    (lexical-let* ((project-name (plist-get blog :project-name))
        (project-command (assoc project-name org-publish-project-alist))
        (project-blogs (plist-get (cdr project-command) :components))
        (updated-project-blogs (append (list command-name) project-blogs)))
      (setq org-publish-project-alist
         (remove project-command org-publish-project-alist))

      (add-to-list
       'org-publish-project-alist
       (list project-name
          :components updated-project-blogs)))
    nil))

;; Selection
(defun org-jekyll-blogger--get-project-names ()
  "Get current project names from `org-jekyll-blogger--projects'."
  (hash-table-keys org-jekyll-blogger--projects))

(defun org-jekyll-blogger--get-blog-names-by-project-name (project-name)
  "Get current blogs names from `org-jekyll-blogger--blogs'."
  (mapcar
   (lambda (blog)
     (plist-get blog :blog-name))
   (cl-remove-if-not
    (lambda (blog)
      (string=
       (plist-get blog :project-name)
       project-name))
    (hash-table-values org-jekyll-blogger--blogs))))

(defun org-jekyll-blogger--get-active-project-names ()
  "Like `org-jekyll-blogger--get-project-names' but filters only ones with linked blogs."
  (cl-remove-if
   (lambda (project-name)
     (null (org-jekyll-blogger--get-blog-names-by-project-name project-name)))
   (org-jekyll-blogger--get-project-names)))




(defun org-jekyll-blogger--completing-read (prompt collection &rest args)
  "Completing read to support alist collections without invoking other libraries."
  (lexical-let ((result
       (apply #'completing-read
          (append (list prompt collection) args))))
    (if (org-jekyll-blogger--alist-p collection)
        (cdr (assoc result collection))
      result)))

(defun org-jekyll-blogger-read-project ()
  (lexical-let ((active-project-names
       (org-jekyll-blogger--get-active-project-names)))
    (unless active-project-names
      (error "No projects with blogs defined yet"))

    (lexical-let* ((project-name
         (org-jekyll-blogger--completing-read
          "Select a project: "
          active-project-names
          nil
          t))
        (project
         (gethash project-name org-jekyll-blogger--projects)))
      project)))

(defun org-jekyll-blogger-read-project-and-blog ()
  "Select a project and blog and output it as pair."
  (lexical-let ((active-project-names
       (org-jekyll-blogger--get-active-project-names)))
    (unless active-project-names
      (error "No projects with blogs defined yet"))

    (lexical-let* ((project-name
         (org-jekyll-blogger--completing-read
          "Select a project: "
          active-project-names
          nil
          t))
        (project
         (gethash project-name org-jekyll-blogger--projects))

        (blog-name
         (org-jekyll-blogger--completing-read
          "Select a blog: "
          (org-jekyll-blogger--get-blog-names-by-project-name project-name)
          nil
          t))
        (blog
         (gethash
          (org-jekyll-blogger--blog-key project-name blog-name)
          org-jekyll-blogger--blogs)))
      (cons project blog))))

(defun org-jekyll-blogger--read-post ()
  "Find an draft or post."
  (pcase-let* ((`(,_ . ,blog)
                (org-jekyll-blogger-read-project-and-blog))
               (draft-posts
                (org-jekyll-blogger--blog-drafts blog))
               (posted-posts
                (cl-sort
                 (org-jekyll-blogger--blog-posts blog)
                 (lambda (x y)
                   (string>
                    (cadr x)
                    (cadr y)))))
               (posts
                (append
                 (mapcar
                  (lambda (draft)
                    (cons (format "(%10s) %s" "" (car draft))
                       (cdr draft)))
                  draft-posts)
                 (mapcar
                  (lambda (posted)
                    (cons (format "(%10s) %s" (cadr posted) (car posted))
                       (caddr posted)))
                  posted-posts))))
    (unless posts
      (error "No posts for the blog yet"))

    (lexical-let* ((selected-post
         (org-jekyll-blogger--completing-read
          "Select a post: "
          posts
          nil
          t)))
      selected-post)))


;; Drafts
(defun* org-jekyll-blogger--define-post (title blog)
  "Define a post."
  (list
   :title (string-trim title)
   :options
   (org-jekyll-blogger--plist-merge
    (list :title title)
    (plist-get blog :post-options))
   :headers
   (plist-get blog :post-headers)))

(defun org-jekyll-blogger--blog-drafts (blog)
  "Get the current drafts of a BLOG."
  (mapcar
   (lambda (post)
     (cons (file-name-base post)
        post))
   (org-jekyll-blogger--dir-posts
    (org-jekyll-blogger--blog-draft-dir blog))))

(defun org-jekyll-blogger--blog-posts (blog)
  "Get the current drafts of a BLOG."
  (cl-remove-if
   (lambda (triple)
     (let ((file-name (caddr triple)))
       (and (backup-file-name-p file-name)
          (auto-save-file-name-p file-name))))
   (mapcar
    (lambda (post)
      (list
       (substring-no-properties
        (file-name-base post)
        11 ;; Remove base date with - `yyyy-mm-dd-'
        )
       (substring-no-properties
        (file-name-base post)
        0 10)
       post))
    (org-jekyll-blogger--dir-posts
     (org-jekyll-blogger--blog-post-dir blog)))))

(defun org-jekyll-blogger--dir-posts (dir)
  "Get the posts of an DIR."
  (mapcar
   (lambda (post-name)
     (expand-file-name post-name dir))
   (let ((default-directory dir))
     (file-expand-wildcards (concat "*" org-jekyll-blogger--post-ext)))))

(defun org-jekyll-blogger-create-draft ()
  "Create a draft."
  (interactive)
  (pcase-let ((`(,_ . ,blog)
               (org-jekyll-blogger-read-project-and-blog))
              (draft-name
               (read-string
                "Enter draft name: "
                nil
                nil
                nil)))
    (lexical-let* ((draft-dir (org-jekyll-blogger--blog-draft-dir blog))
        (draft-file
         (expand-file-name
          (concat draft-name org-jekyll-blogger--post-ext)
          draft-dir))
        (post
         (org-jekyll-blogger--define-post draft-name blog)))
      (find-file draft-file)

      (insert (org-jekyll-blogger--format-post-preamble post))
      (insert "\n\n")

      (message "%s drafted" draft-name))))

(defun org-jekyll-blogger--post-draft-prefix ()
  "Get the time format for posting a draft."
  (format-time-string "%Y-%m-%d"))

(defun org-jekyll-blogger--move-file (file new-location)
  "Write this FILE to NEW-LOCATION, and delete the old one."
  (when (file-exists-p new-location)
    (delete-file new-location))

  (with-current-buffer (find-file-noselect file)
    (write-file new-location t))

  (when (and file
           (file-exists-p new-location)
           (not (string-equal file new-location)))
    (delete-file file)))

(defun org-jekyll-blogger-post-draft ()
  "Post a draft."
  (interactive)
  (pcase-let* ((`(,_ . ,blog)
                (org-jekyll-blogger-read-project-and-blog))
               (draft-posts
                (org-jekyll-blogger--blog-drafts blog)))
    (unless draft-posts
      (error "No drafts for the blog yet"))

    (lexical-let* ((draft-post
         (org-jekyll-blogger--completing-read
          "Select a draft: "
          (mapcar
           (lambda (post)
             (cons (car post) post))
           draft-posts)
          nil
          t))
        (draft-name
         (car draft-post))
        (draft-file
         (cdr draft-post))
        (draft-prefix
         (org-jekyll-blogger--post-draft-prefix))
        (draft-file-name
         (format "%s-%s%s" draft-prefix draft-name org-jekyll-blogger--post-ext))
        (posted-file
         (expand-file-name
          draft-file-name
          (org-jekyll-blogger--blog-post-dir blog))))
      (org-jekyll-blogger--move-file
       draft-file
       posted-file)

      (message "%s posted" draft-name))))

(defun org-jekyll-blogger-find-post ()
  "Find a post(or draft)."
  (interactive)
  (lexical-let* ((selected-post (org-jekyll-blogger--read-post)))
    (find-file selected-post)))


(defun org-jekyll-blogger--publish-file ()
  "Publish current file with `org-publish-current-file'."
  (save-excursion
    (org-publish-current-file)))

;; Writing
(defun org-jekyll-blogger-auto-publish-on-save ()
  "Publish current file whenever saved."
  (interactive)
  (add-hook 'after-save-hook #'org-jekyll-blogger--publish-file t t))


;; Publishing
(defun org-jekyll-blogger-publish-project ()
  "Publish a jekyll project."
  (interactive)
  (lexical-let* ((project (org-jekyll-blogger-read-project))
      (project-command (org-jekyll-blogger--project-command project)))
    (org-publish project-command)))

(defun org-jekyll-blogger-publish-blog ()
  "Publish a jekyll blog."
  (interactive)

  (lexical-let* ((project (org-jekyll-blogger-read-project))
      (project-command (org-jekyll-blogger--project-command project)))
    (org-publish project-command)))

(defun org-jekyll-blogger-publish-post ()
  "Publish a post(or draft)."
  (interactive)
  (lexical-let* ((selected-post (org-jekyll-blogger--read-post)))
    (org-publish-file selected-post)))

(defun org-jekyll-blogger-cleanup-site-drafts (&optional blog)
  "Cleanout a BLOG's site draft directory."
  (interactive)
  (lexical-let* ((the-blog
       (or blog
          (cdr (org-jekyll-blogger-read-project-and-blog))))
      (publish-root
       (plist-get the-blog :blog-publish-root))
      (site-draft-dir
       (expand-file-name
        org-jekyll-blogger--drafts-dir-name
        publish-root)))
    (mapc
     (lambda (file-name)
       (unless (or (string= "." file-name) (string= ".." file-name))
         (delete-file (expand-file-name file-name site-draft-dir) t)))
     (directory-files site-draft-dir))

    (message "%s cleaned." site-draft-dir)))



(defun org-jekyll-blogger--parent (file)
  "Get the parent of a FILE."
  (expand-file-name (concat file "/..")))

(defun org-jekyll-blogger--parent-of (dir file)
  "Check if DIR is a parent of FILE."
  (string-prefix-p (expand-file-name dir) (expand-file-name file)))

(defun org-jekyll-blogger--blog-of (&optional file)
  "Get the blog containing the FILE."
  (lexical-let ((this-blog nil))
    (mapc
     (lambda (blog)
       (lexical-let* ((blog-root (plist-get blog :blog-root))
           (current-file (or file (buffer-file-name))))
         (when (org-jekyll-blogger--parent-of blog-root current-file)
           (setq this-blog blog))))
     (org-jekyll-blogger-blogs))
    this-blog))

(defun org-jekyll-blogger--find-file-hook ()
  "Trigger org-jekyll-blogging find file hook."
  (lexical-let* ((current-file (buffer-file-name))
      (blog (org-jekyll-blogger--blog-of current-file)))
    (when blog
      (lexical-let* ((project
           (gethash (plist-get blog :project-name) org-jekyll-blogger--projects))
          (file-parent
           (file-name-nondirectory
            (org-jekyll-blogger--parent
             current-file)))
          (file-type
           (cond
            ((string= file-parent org-jekyll-blogger--drafts-dir-name)
             'draft)
            ((string= file-parent org-jekyll-blogger--posts-dir-name)
             'post)
            ((string= file-parent org-jekyll-blogger--data-dir-name)
             'data)
            ((string= file-parent org-jekyll-blogger--images-dir-name)
             'image)
            (t 'other))))
        (run-hook-with-args
         'org-jekyll-blogger-find-file-hook
         project
         blog
         file-type
         current-file)))))

(defun org-jekyll-blogger--change-check ()
  "A timer function to change directory."
  (mapc
   (lambda (blog)
     (lexical-let* ((image-dir (org-jekyll-blogger--blog-image-dir blog))
         (current-modification-time
          (file-attribute-modification-time (file-attributes image-dir)))
         (previous-modification-time
          (gethash image-dir org-jekyll-blogger--change-table (current-time))))
       (when (time-less-p previous-modification-time current-modification-time)
         (lexical-let* ((project
              (gethash (plist-get blog :project-name) org-jekyll-blogger--projects))
             (directory-name
              (file-name-nondirectory image-dir))
             (file-type
              (cond
               ((string= directory-name org-jekyll-blogger--drafts-dir-name)
                'draft)
               ((string= directory-name org-jekyll-blogger--posts-dir-name)
                'post)
               ((string= directory-name org-jekyll-blogger--data-dir-name)
                'data)
               ((string= directory-name org-jekyll-blogger--images-dir-name)
                'image)
               (t 'other))))
           (run-hook-with-args
            'org-jekyll-blogger-dir-change-hook
            project
            blog
            file-type
            image-dir)))
       (puthash image-dir current-modification-time org-jekyll-blogger--change-table)))
   (org-jekyll-blogger-blogs)))


(define-minor-mode org-jekyll-blogger-mode
  "Org-Jekyll-Blogger mode."
  :lighter " org-jekyll-blogger "
  :init-value nil
  :global t
  :keymap (make-sparse-keymap)
  (if org-jekyll-blogger-mode
      (progn
        (add-hook 'find-file-hook #'org-jekyll-blogger--find-file-hook t)

        (setq org-jekyll-blogger--change-table
           (make-hash-table :test 'equal)
           org-jekyll-blogger--change-timer
           (run-with-idle-timer 1 t #'org-jekyll-blogger--change-check)))
    (remove-hook 'find-file-hook #'org-jekyll-blogger--find-file-hook)

    (when (timerp org-jekyll-blogger--change-timer)
      (cancel-timer org-jekyll-blogger--change-timer))

    (setq org-jekyll-blogger--change-timer nil)))


(provide 'org-jekyll-blogger)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-jekyll-blogger.el ends here
