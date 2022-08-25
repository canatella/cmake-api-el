;;; cmake-api.el --- CMake api -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'project)
(require 'json)

(defgroup cmake-api nil "CMake file api for Emacs." :group 'convenience)

(defcustom cmake-api-build-directory nil "The CMake build directory."
  :type 'directory
  :group 'cmake-api)

(defconst cmake-api-end-point-relative ".cmake/api/v1/" "The CMake api end point path.")

(defvar cmake-api-reply-cache
  (make-hash-table :test 'equal)
  "A cache to avoid reparsing json too much.")

(defun cmake-api-reply-end-point ()
  "Return the CMake api reply end point."
  (concat cmake-api-build-directory cmake-api-end-point-relative "reply/"))

(defun cmake-api-reply-load-file (file)
  "Load json reply FILE."
  (if-let* ((full-path (concat (cmake-api-reply-end-point) file))
            (cache-entry (gethash full-path cmake-api-reply-cache))
            (cached-at (car cache-entry))
            (content (cdr cache-entry))
            (updated-at (file-attribute-modification-time (file-attributes full-path)))
            (up-to-date (time-less-p updated-at cached-at)))
      content
    (let ((content (json-read-file full-path)))
      (puthash full-path (cons (current-time) content) cmake-api-reply-cache )
      content)))

(defun cmake-api-reply-load-item (item)
  "Load json reply for ITEM."
  (cmake-api-reply-load-file (map-elt item 'jsonFile)))

(defun cmake-api-reply-index-file ()
  "Return the last generated index file."
  (car
   (sort (directory-files (cmake-api-reply-end-point) nil "^index-.*\\.json") #'string-greaterp)))

(defun cmake-api-index ()
  "Return the last generated index."
  (cmake-api-reply-load-file (cmake-api-reply-index-file)))

(defun cmake-api-objects ()
  "Return a list of objects for the current build."
  (map-elt (cmake-api-index) 'objects))

(defun cmake-api-code-model ()
  "Return the code model for the current project, maybe also LOAD-TARGETS."
  (if-let ((object
            (seq-find
             (lambda (object) (string= "codemodel" (map-elt object 'kind)))
             (cmake-api-objects))))
      (cmake-api-reply-load-item object)))

(defun cmake-api-configuration-for-name (code-model name)
  "Fetch configuration NAME from CODE-MODEL."
  (seq-find
   (lambda (configuration) (string= (map-elt configuration 'name) name))
   (map-elt code-model 'configurations)))

(defun cmake-api-configuration-targets-item (configuration)
  "Return targets items for CONFIGURATION."
  (map-elt configuration 'targets))

(defun cmake-api-configuration-targets (configuration)
  "Return targets for CONFIGURATION."
  (seq-map #'cmake-api-reply-load-item (cmake-api-configuration-targets-item configuration)))

(defun cmake-api-configuration-target-for-name (configuration name)
  "Return target named NAME in CONFIGURATION."
  (seq-find
   (lambda (target) (string= name (map-elt target 'name)))
   (cmake-api-configuration-targets configuration)))

(defun cmake-api-target-executable-p (target)
  "Return non nil if TARGET is an executable."
  (string= "EXECUTABLE" (map-elt target 'type)))

(defun cmake-api-target-static-library-p (target)
  "Return non nil if TARGET is an executable."
  (string= "STATIC_LIBRARY" (map-elt target 'type)))

(defun cmake-api-target-sources (target)
  "Return TARGET sources."
  (seq-map
   (lambda (source-item) (map-elt source-item 'path))
   (map-elt target 'sources)))

(defun cmake-api-target-builds-source-p (target source)
  "Return non nil if TARGET builds SOURCE."
  (seq-find
   (lambda (target-source) (string= source target-source))
   (cmake-api-target-sources target)))

(defun cmake-api-target-artifacts (target)
  "Return the list of TARGET artifacts."
  (seq-map
   (lambda (artifact-item) (map-elt artifact-item 'path))
   (map-elt target 'artifacts)))

(provide 'cmake-api)

;;; cmake-api.el ends here
