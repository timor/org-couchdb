;;; org-couchdb.el --- map and synchronize org mode subtrees to couchdb docunments  -*- lexical-binding: t; -*-

;;; Header:

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

;; Provides a means to:
;; - save org entries in couchdb
;; - edit couchdb entries with org modeTools for synchronizing org
;;   subtrees with couchdb.  Treats all entries as properties.
;; ** Idea
;; - provide mapping between org entries (not files) and couchdb documents
;; - an entry with an id (TBD: choose if org-mode id is enough, or
;;   special couchdb id is needed) corresponds to a couch db document
;;   - current choice: couchdb_id
;; - couchdb fields are generally treated as item properties
;; - prime way to identify items is via tags
;; - flexible configuration by just providing properties, using org's
;;   property inheritance
;; - structure of org files is independent of couchdb documents, as the
;;   latter are only stored flat on the server
;; - query results are tagged items, but a special tag->tree
;;   transformation allows to view and store items in a tree
;; ** Conventions
;; - type mappings between json values and org-mode properties:
;;   - per default: a property value will be quoted json
;;   - if you define field types, conversion to and from json values to
;;     property values will be performend on synchronisations
;; - this ensures that in general any document can be rendered and
;;   edited, but also typical and special use-cases are supported
;; ** Issues
;; - currently, interface passes pom around, may want to change that to
;;   (point) if usage is always the same to avoid noise
;; - how should taq handling be performed?
;;   1. only allow org-compatible tags in the database
;;   2. create a mapping from org tags to couchdb tags
;;      1. implicit mapping: define some clever rules (problem: one-way ticket)
;;      2. explicit mapping: have tag descriptions and translations
;;         stored in the database in a special document <- current favourite

;;; Code:


;; #+BEGIN_SRC emacs-lisp
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

;;; Translati

(defun org-couchdb-org-to-json (pom e)
  "Translate an org item to a json document."
  (let ((priority-given (org-element-property :priority e))
	(properties (org-entry-properties pom)))
    ;; return plist
    (mapcar (lambda (p) ) properties)))

(defun org-couchdb-save-entry ()
  "Based on the :COUCHDB_ID: property, post the current entry to couchdb.
  All Properties will be passed as json fields, except for the
  ones where translations have been defined.  The body of the
  entry will be put into the special field 'org-entry-body'.  If
  there is no :COUCHDB_ID:, one will be created, and the property
  will be updated accordingly."
  (let* ((pom (point))
	 (e (org-element-at-point))
	 (id (org-element-property :COUCHDB_ID e))
	 (fields (org-couchdb-org-to-json pom e)))
    ))

(defun org-couchdb-update-entry ()
  "If entry has valid id, query that from the server and update the entry.")

(provide 'org-couchdb)
;;; org-couchdb.el ends here
;; #+END_SRC
