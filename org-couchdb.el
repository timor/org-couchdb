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
(require 'json)
;; #+END_SRC

;; ** Helpers

;; Get the content of an entry, without the property drawer, as raw text
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-get-body (e)
  "Return entry body as raw text.  Needs parse element E."
  (let ((obuf (current-buffer)))
   (with-temp-buffer
     (insert-buffer-substring obuf
			      (org-element-property :contents-begin e)
			      (org-element-property :contents-end e))
     (goto-char (point-min))
     (let ((e (org-element-at-point)))
       ;; unfortunately, the element is parsed as 'drawer, not as
       ;; 'property-drawer when copied to buffer
       (when (and (eq (org-element-type e) 'drawer)
		  (string-equal (org-element-property :drawer-name e) "PROPERTIES"))
	 (goto-char (org-element-property :end e))))
     (buffer-substring-no-properties (point) (point-max)))))
;; #+END_SRC

;; Determine property by either getting it from subtree, buffer, or
;; prompt user.

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-get-property (pom name &optional postprocessor)
  "Determine org property NAME at POM, ask user if not found.
Apply POSTPROCESSOR on the read value."
  (let ((p (or (org-entry-get pom name t)
	       (completing-read (format "Provide value for Property '%s': " name) ()))))
    (if postprocessor
	(funcall postprocessor p)
      p)))
;; #+END_SRC

;; **  Configuration Properties

;; These configure the couchdb connection.  Note that no customization is
;; used, to specify the values, insert =#+PROPERTY: ...= lines.
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-server (pom)
  "Determine the server to use."
  (org-couchdb-get-property pom "couchdb-server"))

(defun org-couchdb-port (pom)
  "Determine the port to use."
  (org-couchdb-get-property pom "couchdb-port" (lambda (s) (string-to-int s))))

(defun org-couchdb-db (pom)
  "Determine the database to use."
  (org-couchdb-get-property pom "couchdb-db"))
;; #+END_SRC

;; ** Translating org to json
;; - type mappings between json values and org-mode properties:
;;   - per default: a property value will be quoted json
;;   - if you define field types, conversion to and from json values to
;;     property values will be performed on synchronisations
;; - this ensures that in general any document can be rendered and
;;   edited, but also typical and special use-cases are supported
;; *** Headline/Entry Translations
;; (WIP)
;; Translation Process: Looks for property-specific translation definitions. If
;; none are found, just simply interpret property as quoted string.
;; Special properties are handled individually.  These include:
;; - org body
;; - deadline
;; - todo state

;; Note that couchdb configuration properties are ignored when writing to the database
;; #+BEGIN_SRC emacs-lisp
;; BUG? "CATEGORY" is not in org-special-properties...
(defvar org-couchdb-ignored-properties
  '("CATEGORY" "COUCHDB-SERVER" "COUCHDB-PORT" "COUCHDB-DB" "COUCHDB-ID" "COUCHDB-REV"))
;; #+END_SRC

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-item-to-json (pom e)
  "Translate an org item to a json document.  Point must be at headline."
  (let ((priority-given (org-element-property :priority e))
	(user-properties (org-entry-properties pom 'standard))
	(special-properties (org-entry-properties pom 'special))
	(field-types (org-couchdb-determine-field-types pom))
	(json-fields ()))
    ;; return plist
    (dolist (p user-properties)
      (if (not (member (car p) org-couchdb-ignored-properties))
	  (push (org-couchdb-property-to-json p field-types) json-fields)))
    json-fields))
;; #+END_SRC

;; *** Property translations
;; By default, all fields are assumed to be quoted strings.
;; #+BEGIN_SRC emacs-lisp
(defvar org-couchdb-property-translations
  '((string (lambda (x) (let ((val (read-from-whole-string x)))
			  (when (not (stringp val))
			    (error "does not evaluate to a string: %s" val))
			  val))
	    (lambda (x) (concat "\"" x "\""))))
    "List of (TYPE ORG>JSON JSON>ORG) mappings.")

(defun org-couchdb-property-to-json (prop field-types)
  "Convert property PROP to plist ready for JSON-encoding, using supplied list of field type definitions FIELD-TYPES."
  (let* ((key (car prop))
	 (org-val (cdr prop))
	 (type (or (cdr (assoc key field-types)) 'string))
	 (translator (or (first (cdr (assoc type org-couchdb-property-translations)))
			 (error "no translation defined for field type %s" type)))
	 (json-val (funcall translator org-val)))
    (cons key json-val)))

(defun org-couchdb-determine-field-types (pom)
  "Determine the list of field type mappings for the entry at POM.  Point must be at headline"
  nil)
;; #+END_SRC
;; *** Strategy Notes
;; **** Org Body
;; At the moment, entries are copied to a temporary buffer in order to
;; extract the body.  The Property drawer is removed by hand.

;; An Alternative would involve parsing the whole buffer with
;; org-element, and then performing all extraction operations on the
;; already parsed tree.  This should be faster for full-buffer
;; synchronizations, but may incur unneccesary parsing overhead.
;; **** Updating unchanged Items
;; At the moment, when an item is submitted to CouchDB, all properties
;; are updated, regardless of change status.  Introducing checksums could
;; be introduced to only synchronize when necessary.  This must be
;; weighed against the overhead of actually checking for up-to-date-ness.
;; **** Nested Items
;; There is no special handling of nested items.  For an outer item, the
;; whole subtree is saved as org-body.  This is true for the inner item,
;; too.  Thus, fetching an outer item with a database link will
;; "instantiate" any inner items, that can have their own database
;; links.

;; Care must be taken with the order of update operations, so that the
;; whole structure is synchronized correctly.

;; In the future, it may be better to explicitely "cut out" the inner
;; linked items, save them in a relation and provide explicit support for
;; "re-instantiating" them when the outer item is updated.
;; **** property-names
;; Since property names are converted to field names, and some special
;; property names like "ORG-BODY" are used, collisions are possible
;; there.

;; This can be fixed by namespacing or scoping, but then the document on
;; the server may become less pleasant to work with.

;; The most likely approach involves creating a special =org= field to
;; separate internal properties from user-defined ones.

;; Current Layout:
;; #+BEGIN_SRC json
;; { "_rev" : "1-deadbeef",
;;   "_id" : "checkmatefool",
;;   "org-body" : "the body stuff",
;;   "deadline": "deadline",
;;   "org-deadline" : "<org-deadline>" }
;; #+END_SRC

;; Proposed Layout:
;; #+BEGIN_SRC json
;; { "_rev" : "1-deadbeef",
;;   "_id" : "checkmatefool",
;;   "deadline": "deadline",
;;   "org" : {
;;       "body" : "the body stuff",
;;       "deadline" : "<org-deadline>" }
;; }
;; #+END_SRC

;; ** Database Commands

;; Saving an entry:
;; - look for =:couchdb-id:= property
;;   - if found, translate and update server document
;;   - if not found, create new server document, save new id
;; Updateing an entry:
;; - loog for =:couchdb-id:= property
;;   - if found, update entry from server document

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-save-entry ()
  "Based on the :couchdb-id: property, post the current entry to couchdb.
  All Properties will be passed as json fields, except for the
  ones where translations have been defined.  The body of the
  entry will be put into the special field 'org-entry-body'.  If
  there is no :couchdb-id:, one will be created, and the property
  will be updated accordingly."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((pom (point))
	   (e (org-element-at-point))
	   (id (org-element-property :COUCHDB-ID e))
	   (rev (org-element-property :COUCHDB-REV e))
	   (body (org-couchdb-get-body e))
	   (fields (acons "ORG-BODY" body (org-couchdb-item-to-json pom e)))
	   (doc (if rev
		    (acons "_rev" rev fields)
		  fields))
	   (couchdb-host (org-couchdb-server pom))
	   (couchdb-port (org-couchdb-port pom))
	   (response (couchdb-doc-save (org-couchdb-db pom)
				       doc id))
	   (new-id (cdr (assoc 'id response)))
	   (new-rev (cdr (assoc 'rev response))))
      (unless (eq (cdr (assoc 'ok response)) t)
	(error "CouchDB request error, Reason: %s" (cdr (assoc 'reason response))))
      (when (and id (not (equal id new-id)))
	(error "Server document ID differs from previously known ID"))
      (org-entry-put pom "COUCHDB-ID" new-id)
      (org-entry-put pom "COUCHDB-REV" new-rev))))
;; #+END_SRC

;; Updating existing Entry.
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-update-entry ()
  "If entry has valid id, query that from the server and update the entry.")

;; #+END_SRC

;; ** Footer
;; #+BEGIN_SRC emacs-lisp

(provide 'org-couchdb)
;;; org-couchdb.el ends here
;; #+END_SRC
