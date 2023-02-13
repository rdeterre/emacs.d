;;; org-drawio.el --- Drawio support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Romain Deterre

;; Author: Romain <romain@deterre.fr>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (f "0.20.0") (org "9.3"))
;; URL: https://github.com/lepisma/org-drawio

;;; Commentary:

;; Inspired by @lepisma's org-krita, this adds draw.io/diagrams.net
;; support for Org Mode
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'arc-mode)
(require 'filenotify)
(require 'f)
(require 'cl-lib)
(require 'org)

(org-link-set-parameters "drawio" :follow #'org-drawio-edit :export #'org-drawio-export)

;; NOTE: Only single reference for a file supported as of now.

(defgroup org-drawio nil
  "Org-drawio customization."
  :group 'org
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-executable (if (eq system-type 'darwin)
                                     "/Applications/draw.io.app/Contents/MacOS/draw.io"
                                     "drawio")
  "The command of drawio or full path to executable."
  :type 'string
  :safe #'stringp
  :group 'org-drawio
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-append-ext t
  "Append automatically .drawio extension."
  :group 'org-drawio
  :type 'boolean
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-get-new-filepath (lambda () (read-file-name "New drawio file: "))
  "Function returning filepath of new created image."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))

(defvar-local org-drawio-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar-local org-drawio-overlays nil
  "A-list mapping file names to overlay.")

(defconst org-drawio-dir (file-name-directory load-file-name)
  "Base directory for package.")

(defun org-drawio-resource (file)
  "Return full path of a resource FILE."
  (expand-file-name file (file-name-as-directory (concat org-drawio-dir "resources"))))

(defun org-drawio-export (_path _desc _backend)
  "Export drawio canvas _PATH from Org files.
Argument _DESC refers to link description.
Argument _BACKEND refers to export backend."
  (let ((png-path (f-swap-ext _path "png")))
    (cl-case _backend
      (html (format "<img src=\"%s\">"
                    (prog1 png-path
                      (org-drawio-save-image _path png-path))))
      (ascii (format "%s (%s)" (or _desc _path) _path))
      (latex (format "\\includegraphics[width=\\textheight,height=\\textwidth,keepaspectratio]{%s}"
                     (prog1 png-path
                       (org-drawio-save-image _path png-path)))))))

(defun org-drawio-save-image (drawio-path png-path)
  "Extract from DRAWIO-PATH a .png and write it to PNG-PATH."
  (let ((image (create-image (org-drawio-extract-png DRAWIO-path) 'png t)))
    (with-temp-buffer
      (insert (plist-get (cdr image) :data))
      (write-region (point-min) (point-max) png-path))))

(defun org-drawio-make-new-image (output-drawio-path &optional width height)
  "Create a new image based on a template at OUTPUT-DRAWIO-PATH."
  (let ((template (org-drawio-resource "template.drawio")))
    ;; TODO: Change image width and height based on provided argument
    (f-copy template output-drawio-path)))

(defun org-drawio-extract-png (drawio-path)
  "Extract png from given DRAWIO-PATH and return data."
  (let ((png-path (f-swap-ext drawio-path "png")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (call-process org-drawio-executable nil nil nil "-x" "-f" "png" "-o" png-path drawio-path)
      (insert-file-contents png-path)
      (buffer-string))))

(defun org-drawio-get-links ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "drawio")
        link))))

(defun org-drawio-event-file-path (event)
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

(defun org-drawio-watcher-callback (event)
  "Callback that runs after drawio files are modified."
  (let* ((drawio-path (org-drawio-event-file-path event))
         (links (org-drawio-get-links))
         (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
         (idx (cl-position drawio-path paths :test #'string-equal)))
    (when idx (org-drawio-show-link (nth idx links)))))

(defun org-drawio-add-watcher (drawio-path)
  "Setup auto-refreshing watcher for given drawio LINK."
  (let ((desc (file-notify-add-watch drawio-path '(change) #'org-drawio-watcher-callback)))
    (unless (alist-get drawio-path org-drawio-watchers nil nil #'string-equal)
      (push (cons drawio-path desc) org-drawio-watchers))))

(defun org-drawio-edit (path &optional full-mode)
  "Edit given PATH in drawio canvasonly mode.

If FULL-MODE is not null, run full drawio."
  (let ((drawio-path (expand-file-name path)))
    (when (f-exists-p drawio-path)
      (if full-mode
          (call-process org-drawio-executable nil nil nil drawio-path)
        (call-process org-drawio-executable nil nil nil "--canvasonly" "--nosplash" drawio-path))
      (org-drawio-add-watcher drawio-path))))

(defun org-drawio-hide-link (link)
  (let ((overlay (alist-get (org-element-property :path link) org-drawio-overlays nil nil #'string-equal)))
    (when overlay (delete-overlay overlay))))

(defun org-drawio-show-link (link)
  (org-drawio-hide-link link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (drawio-path (org-element-property :path link)))
    (overlay-put overlay 'display (create-image (org-drawio-extract-png drawio-path) 'png t))
    (push (cons drawio-path overlay) org-drawio-overlays)))

(defun org-drawio-hide-all ()
  (dolist (link (org-drawio-get-links))
    (org-drawio-hide-link link)))

(defun org-drawio-enable ()
  (dolist (link (org-drawio-get-links))
    (org-drawio-show-link link)))

(defun org-drawio-disable ()
  "Disable watchers and hide drawio images."
  (dolist (watcher org-drawio-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-drawio-watchers nil)
  (org-drawio-hide-all))

(defun org-drawio-validate-path (path)
  "Validate the file PATH as a drawio path."
  (if (f-ext-p path "drawio")
      path
    (if org-drawio-append-ext
        (concat path ".drawio")
      path)))

;;;###autoload
(defun org-drawio-insert-new-image (output-drawio-path desc)
  "Insert new image in current buffer."
  (interactive
   (let ((output-drawio-path (funcall org-drawio-get-new-filepath))
         (desc (funcall org-drawio-get-new-desc)))
     (list (org-drawio-validate-path output-drawio-path) desc)))
  (org-drawio-make-new-image output-drawio-path)
  (org-insert-link nil (concat "drawio:" output-drawio-path) desc)
  ;; TODO: Enable only the new image
  (org-drawio-enable))

;;;###autoload
(define-minor-mode org-drawio-mode
  "Mode for displaying editable drawio images within Org file."
  :init-value nil
  (if org-drawio-mode (org-drawio-enable) (org-drawio-disable)))

(provide 'org-drawio)

;;; org-drawio.el ends here
