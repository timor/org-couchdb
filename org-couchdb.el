;;; org-couchdb.el --- map and synchronize org mode subtrees to couchdb docunments  -*- lexical-binding: t; -*-

;; Author: timor <timor.dd@googlemail.com>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
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

;; Tools for synchronizing org subtrees with couchdb.  Treats all entries as properties.

;;; Code:

(require 'couchdb)

;;; Helper: determine property by either getting it from subtree, buffer or prompt
(defun org-couchdb-get-property (pom name &optional postprocessor)
  "Determine org property NAME at POM, ask user if not found.
Apply POSTPROCESSOR on the read value."
  (let ((p (or (org-entry-get pom name t)
	       (completing-read (format "Provide value for Property '%s': " name) ()))))
    (if postprocessor
	(funcall postprocessor p)
      p)))

(defun org-couchdb-server (pom)
  "Determine the server to use."
  (org-couchdb-get-property pom "couchdb-server"))

(defun org-couchdb-port (pom)
  "Determine the port to use."
  (org-couchdb-get-property pom "couchdb-port" (lambda (s) (string-to-int s))))

(defun org-couchdb-org-to-json (pom e)
  "Translate an org item to a json document".
  ())

(defun org-couchdb-save-entry ()
  "Based on the :COUCHDB_ID: property, post the current entry to couchdb.
  All Properties will be passed as json fields, except for the
  ones where translations have been defined.  The body of the
  entry will be put into the special field 'org-entry-body'.  If
  there is no :COUCHDB_ID:, one will be created, and the property
  will be updated accordingly."
  (let* ((pom (point))
	 (e (org-element-at-point))
	 (priority-given (org-element-property :priority e))
	 (id (org-element-property :COUCHDB_ID))
	 (fields (org-couchdb-org-to-json pom e)))
    ))

(defun org-couchdb-update-entry ()
  "If entry has valid id, query that from the server and update the entry.")

(provide 'org-couchdb)
;;; org-couchdb.el ends here
