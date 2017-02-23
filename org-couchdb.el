;;; org-couchdb.el --- map and synchronize org mode subtrees to couchdb docunments  -*- lexical-binding: t; -*-

;; #+STARTUP: indent nil

;;; Header:

;; Author: timor <timor.dd@googlemail.com>
;; Keywords: comm

;;; Commentary:

;; Provides a means to:
;; - store org entries in couchdb
;; - edit couchdb entries with org modeTools for synchronizing org
;;   subtrees with couchdb.  Treats all entries as properties.
;; ** Idea
;; - provide mapping between org entries (not files) and couchdb documents
;; - an entry with a coucdb id corresponds to a couch db document
;; - couchdb fields are generally treated as item properties
;; - primary way to identify items is via tags
;; - flexible configuration by just providing properties, using org's
;;   property inheritance
;; - structure of org files is independent of couchdb documents, as the
;;   latter are only stored flat on the server
;; - query results are tagged items, but a special tag->tree
;;   transformation allows to view and store items in a tree
;; ** Setup
;; *** Configuration
;; All configuration is done via org-mode properties.  The minimum setup
;; to get started is:
;; #+BEGIN_EXAMPLE org
;; ,#+PROPERTY: couchdb-server <server-name-or-ip>
;; ,#+PROPERTY: couchdb-port <server-port>
;; ,#+PROPERTY: couchdb-db <database-name>
;; #+END_EXAMPLE

;; Note that currently the database must already exist on the server.
;; ** Strategy Notes
;; *** Org Body
;; At the moment, entries are copied to a temporary buffer in order to
;; extract the body.  The Property drawer is removed by hand.

;; An Alternative would involve parsing the whole buffer with
;; org-element, and then performing all extraction operations on the
;; already parsed tree.  This should be faster for full-buffer
;; synchronizations, but may incur unneccesary parsing overhead.
;; *** Updating unchanged Items
;; At the moment, when an item is submitted to CouchDB, all properties
;; are updated, regardless of change status.  Introducing checksums could
;; be introduced to only synchronize when necessary.  This must be
;; weighed against the overhead of actually checking for up-to-date-ness.
;; *** Nested Items
;; There is no special handling of nested items.  For an outer item, the
;; whole subtree is stored as org-body.  This is true for the inner item,
;; too.  Thus, fetching an outer item with a database link will
;; "instantiate" any inner items, that can have their own database
;; links.

;; Care must be taken with the order of update operations, so that the
;; whole structure is synchronized correctly.

;; In the future, it may be better to explicitely "cut out" the inner
;; linked items, save them in a relation and provide explicit support for
;; "re-instantiating" them when the outer item is updated.
;; *** Property Names
;; Since property names are converted to field names, and some special
;; property names like "ORG-BODY" are used, collisions are possible
;; there.

;; This can be fixed by namespacing or scoping, but then the document on
;; the server may become less pleasant to work with.
;; *** Special Properties
;; **** Alternative Approach: scope out org properties
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
;; **** Alternative Approach: mapping between fields and special items
;; In this approach, the user can easily configure which fields are to be
;; interpreted as special fields.  E.g. a property couchdb-org-body-field
;; would be set to "content" per default, but overridable with an org
;; property itself (TBD: define wether that configuration is stored
;; alongside the document, possibly in the above configuration

;; ** Issues
;; - currently, interface passes pom around, may want to change that to
;;   (point) if usage is always the same to avoid noise
;; - how should tag handling be performed?
;;   1. only allow org-compatible tags in the database
;;   2. create a mapping from org tags to couchdb tags
;;      1. implicit mapping: define some clever rules (problem: one-way ticket)
;;      2. explicit mapping: have tag descriptions and translations
;;         stored in the database in a special document <- current favourite
;; *** Attachments
;; 2 Problems:

;; 1. Saving Attachments
;; 2. Fetching Attachments

;; Possible solutions:
;; - Create special =:ATTACHMENTS:= drawer
;; - manually walk over file links
;; - use org's attachment facility <- current choice
;; *** Bulk updates
;; - currently, no bulk updated are performed
;; - this should use CouchDB's native support for bulk updates
;; - probably only makes sense once some form of caching is implemented

;;; Code:


;; #+BEGIN_SRC emacs-lisp
(require 'org)
(require 'org-attach)
(require 'couchdb)
(require 'request)
(require 'json)
;; #+END_SRC

;; ** Variables
;; #+BEGIN_SRC emacs-lisp
(defvar org-couchdb-request-timeout 5
  "Timeout in seconds for synchronous requests made with request.el")
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

;; Compute the md5 sum of a file.  Expects the program =md5sum= to be in
;; path.

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-md5sum (file)
  "Compute md5sum of FILE.  Need program \"md5sum\" in path"
  (if (file-exists-p file)
      (first (split-string (shell-command-to-string (concat "md5sum " file))))
    (error "file does not exist: %s" file)))
;; #+END_SRC


;; *** HTTP Requests
;; HTTP access requests.  Implement those here that are not provided by
;; couchdb.el.  Might switch to request.el for all of them eventually.

;; Perform a HEAD request.

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-head-request (url)
  "Perform synchronous head request.  Returns request-response object."
  (request url :type "HEAD" :sync t :timeout org-couchdb-request-timeout))
;; #+END_SRC

;; #+BEGIN_SRC emacs-lisp
;; Perform a PUT request with a file as data
(defun org-couchdb-put-file (url file &optional content-type)
  "Perform synchronous put request to URL, sending FILE.  Returns request-response object.  Uses curl."
  (let ((cmd (string-join (list "curl" "-s" "-X PUT"
				url (concat "--data-binary @" file)
				(when content-type
				  (concat "-H \"Content-Type:" content-type "\""))) " ")))
    (message "executing: %s" cmd)
    (json-read-from-string (shell-command-to-string cmd))))
;; #+END_SRC

;; ** Configuration Properties
;; All Configuration is done using properties, in addition to the
;; variable =org-couchdb-property-defaults=.

;; #+BEGIN_SRC emacs-lisp
(defvar org-couchdb-property-defaults
  '(("couchdb-port" . "5984")
    ("couchdb-org-body-field" . "content")
    ("couchdb-org-title-field" . "title")
    ("couchdb-org-deadline-field" . "deadline")
    ("couchdb-field-type" . "")
    ("couchdb-ignore-properties" . "")))

;; Determine property by either getting it from subtree, buffer, or
;; prompt user.

(defun org-couchdb-get-property (pom name &optional postprocessor)
  "Determine org property NAME at POM, ask user if not found.
Apply POSTPROCESSOR on the read value."
  (let ((p (or (org-entry-get pom name t)
	       (cdr (assoc name org-couchdb-property-defaults))
	       (completing-read (format "Provide value for Property '%s': " name) nil))))
    (if postprocessor
	(funcall postprocessor p)
      p)))
;; #+END_SRC

;; Note that couchdb configuration properties are ignored when writing to the database
;; #+BEGIN_SRC emacs-lisp
;; BUG? "CATEGORY" is not in org-special-properties...
(defvar org-couchdb-ignored-properties-builtin
  '("CATEGORY" "COUCHDB-SERVER" "COUCHDB-PORT" "COUCHDB-DB" "COUCHDB-ID" "COUCHDB-REV"
    "COUCHDB-ORG-TITLE-FIELD" "COUCHDB-ORG-BODY-FIELD" "COUCHDB-ORG-DEADLINE-FIELD" "ATTACHMENTS"))

(defun org-couchdb-ignored-properties (pom)
  (mapcar 'upcase (append org-couchdb-ignored-properties-builtin
			  (split-string (org-couchdb-get-property pom "couchdb-ignore-properties")))))
;; #+END_SRC

;; ** Connection

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

;; *** Field Type mappings
;;     :PROPERTIES:
;;     :ID:       3cb99f22-42e1-44eb-a175-a4a592f2082f
;;     :END:
;; Per default, all fields (except for some special builtins like REV and
;; ID) are treated as literal quoted json properties.

;; This can be overridden by providing mappings in the org property
;; =couchdb-field-type=, like this:

;; #+BEGIN_EXAMPLE org
;; ,#+PROPERTY: couchdb-field-type foo string
;; ,#+PROPERTY: couchdb-field-type+ bar number
;; ,#+PROPERTY: couchdb-field-type+ prefix_.* string
;; #+END_EXAMPLE

;; This uses org mode's facility of adding values to properties.  Each
;; entry is of the form
;; #+BEGIN_EXAMPLE
;; regexp type-symbol
;; #+END_EXAMPLE

;; Note that the first match is applied.  If more than one rule applies,
;; the first takes precedence.

;; Example:
;; Withouth any mappings, the following json
;; #+BEGIN_EXAMPLE json
;; { "foo" : "bar" }
;; #+END_EXAMPLE

;; translates to the property drawer:
;; #+BEGIN_EXAMPLE org
;; :PROPERTIES:
;; :foo: "bar"
;; :END:
;; #+END_EXAMPLE

;; Defining the following beforehand
;; #+BEGIN_EXAMPLE org
;; ,#+PROPERTY: foo string
;; #+END_EXAMPLE

;; will cause the property to be written and read like this:
;; #+BEGIN_EXAMPLE org
;; :PROPERTIES:
;; :foo: bar
;; :END:
;; #+END_EXAMPLE

;; Note that is also possible to simply specify more than one mapping in
;; a =#+PROPERTY:= directive:
;; #+BEGIN_EXAMPLE org
;; ,#+PROPERTY: couchdb-field-type foo string bar number prefix_.* string
;; #+END_EXAMPLE

;; As usual, these properties can be overridden on subtree or entry properties.

;; TODO: the following should probably be optimized if it proves a bottleneck, since it does a lot of string matching for each(!) property
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-field-type (pom field)
  "Try to determine any override for field type of FIELD at POM.  Return nil if no override was found."
  (let ((tokens (split-string (org-couchdb-get-property pom "couchdb-field-type"))))
    (unless (evenp (length tokens)) (error "Entries of `couchdb-field-type' must be <regex> <type-symbol> pairs"))
    (loop for l = tokens then (cddr l)
	  for (re type &rest) = l
	  while l do
	  (when (string-match re field)
	    (return (intern type))))))
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

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-item-to-json (pom e)
  "Translate an org item to a json document.  Point must be at headline."
  (let ((priority-given (org-element-property :priority e))
	(user-properties (org-entry-properties pom 'standard))
	(special-properties (org-entry-properties pom 'special))
	(json-fields ()))
    ;; return plist
    (dolist (p user-properties)
      (unless (member (car p) (org-couchdb-ignored-properties pom))
	(push (org-couchdb-property-to-json p (org-couchdb-field-type pom (car p))) json-fields)))
    json-fields))
;; #+END_SRC

;; *** Property translations
;; By default, all fields are assumed to be quoted strings representing
;; json values.  See [[id:3cb99f22-42e1-44eb-a175-a4a592f2082f][Field Type mappings]] for details.
;; #+BEGIN_SRC emacs-lisp
(defvar org-couchdb-property-translations
  '((quoted-json (lambda (x) (let ((val (read-from-whole-string x)))
				(when (not (stringp val))
				  (error "Does not evaluate to a quoted string: %s" val))
				val))
		  (lambda (x) (concat "\"" x "\"")))
    (string identity identity))
  "List of (TYPE ORG>JSON JSON>ORG) mappings.")
;; #+END_SRC

;; To be able to connect to couchdb instances, An org item needs to be
;; translated to JSON.  Since this is eventually done by the couchdb
;; interface,  we only convert an item to an intermediate representation,
;; which is a property list ready for JSON encoding.

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-property-to-json (prop field-type)
  "Convert property PROP to plist ready for JSON-encoding, using supplied field type FIELD-TYPE.  If FIELD-TYPE is nil, PROP will be treated as quoted json"
  (let* ((key (car prop))
	 (org-val (cdr prop))
	 (type (or field-type 'quoted-json))
	 (translator (or (first (cdr (assoc type org-couchdb-property-translations)))
			 (error "no translation defined for field type %s" type)))
	 (json-val (funcall translator org-val)))
    (cons (downcase key) json-val)))

;; #+END_SRC
;; ** Database Commands
;; Interactive commands all move point to the current entry.

;; TODO: factor out common code of store and fetch code.

;; *** Helper Macros

;; #+BEGIN_SRC emacs-lisp
(defmacro org-couchdb-with-entry (point-var &rest body)
  "Jump to beginning of entry for BODY, with POINT-VAR bound to the current point."
  (declare (indent 1)
	   (debug (sexp body)))
  `(save-excursion
     (org-back-to-heading)
     (let ((,point-var (point)))
       ,@body)))
;; #+END_SRC

;; couchdb.el expects the host and port in dynamic variables.
;; Setting these based on the current item is a recurring task.

;; #+BEGIN_SRC emacs-lisp
(defmacro org-couchdb-with-current-host (&rest body)
  "Set the variables for couchdb.el based on the current buffer position."
  (declare (indent 1) (debug (body)))
  `(let ((couchdb-host (org-couchdb-server nil))
	 (couchdb-port (org-couchdb-port nil)))
     ,@body))
;; #+END_SRC

;; *** Storing an entry
;; - look for =:couchdb-id:= property
;;   - if found, translate and update server document
;;   - if not found, create new server document, save new id
;; Updating an entry:
;; - look for =:couchdb-id:= property
;;   - if found, update entry from server document

;; (defun org-couchdb-update-rev (new-rev)
;;   "Change the couchdb rev of item at point to NEW-REV."
;;   (org-entry-put pom "COUCHDB-REV" new-rev))

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-store-entry ()
  "Based on the :couchdb-id: property, post the current entry to couchdb.
  All Properties will be passed as json fields, except for the
  ones where translations have been defined.  The body of the
  entry will be put into the special field 'org-entry-body'.  If
  there is no :couchdb-id:, one will be created, and the property
  will be updated accordingly."
  (interactive)
  (org-couchdb-with-entry pom
    (let ((current-item (org-get-heading t t)))
		       (message "Syncing: %s" current-item))
    (let* ((e (org-element-at-point))
	   (id (org-element-property :COUCHDB-ID e))
	   (rev (org-element-property :COUCHDB-REV e))
	   (body (org-couchdb-get-body e))
	   (title (org-element-property :title e))
	   (generated-fields (list (cons (org-couchdb-get-property pom "couchdb-org-body-field") body)
				   (cons (org-couchdb-get-property pom "couchdb-org-title-field") title)))
	   (fields (append generated-fields (org-couchdb-item-to-json pom e)))
	   (doc (if rev
		    (acons "_rev" rev fields)
		  fields))
	   ;; (couchdb-host (org-couchdb-server pom))
	   ;; (couchdb-port (org-couchdb-port pom))
	   (response (org-couchdb-with-current-host
			 (couchdb-doc-save (org-couchdb-db pom) doc id)))
	   (new-id (cdr (assoc 'id response)))
	   (new-rev (cdr (assoc 'rev response))))
      (unless (eq (cdr (assoc 'ok response)) t)
	(error "CouchDB request error, Reason: %s" (cdr (assoc 'reason response))))
      (when (and id (not (equal id new-id)))
	(error "Server document ID differs from previously known ID"))
      (org-entry-put pom "COUCHDB-ID" new-id)
      (org-couchdb-update-rev new-rev))))
;; #+END_SRC

;; *** TODO Updating an existing Entry
;; #+BEGIN_SRC emacs-lisp

(defun org-couchdb-fetch-entry ()
  "If entry has valid id, query that from the server and update the entry."
  (interactive)
  (org-couchdb-with-entry pom
    (let* ((e (org-element-at-point))
	   (id (or (org-element-property :COUCHDB-ID e)
		   (error "Item does not have COUCHDB-ID property, cannot fetch from server.")))
	   ;; (couchdb-host (org-couchdb-server pom))
	   ;; (couchdb-port (org-couchdb-port pom))
	   (response (org-couchdb-with-current-host
			 (couchdb-doc-info (org-couchdb-db pom) id)))
	   (db-error (cdr (assoc 'error response)))
	   (new-id (cdr (assoc '_id response)))
	   (new-rev (cdr (assoc '_rev response))))
      (when db-error
	(error "CouchDB request error, Reason: %s" (cdr (assoc 'reason response))))
      (when (and id (not (equal id new-id)))
	(error "Server document ID differs from previously known ID")))))
;; #+END_SRC
;; *** Attachments
;; Org attachments are stored as couchdb attachments.  To prevent
;; unnecessary transfers, checksums are compared with existing
;; attachments before uploading.

;; Construct a url to an items attachment

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-attachment-url (db id attachment)
  "Return url to attachment."
  (format "http://%s:%s/%s/%s/%s" (org-couchdb-server nil) (org-couchdb-port nil)
		      db id (url-hexify-string attachment)))
;; #+END_SRC

;; Collect info about an attachment on the server.

;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-attachment-info (db id attachment)
  "Return a plist of information about ATTACHMENT of doc ID in
database DB.  Returns nil if the server responds with an error."
  (let* ((url (org-couchdb-attachment-url db id attachment))
	 (response (org-couchdb-head-request url)))
    (unless (>= (request-response-status-code response) 400)
      (let* ((digest (request-response-header response "Content-MD5"))
	     (md5sum (loop for c across (base64-decode-string digest) concat (format "%02x" c)))
	     (encoding (request-response-header response "Content-Encoding"))
	     (length (string-to-number (request-response-header response "Content-Length")))
	     ;; NOTE: undocumented???
	     (content-type (request-response-header response "Content-Type")))
	(list :md5sum md5sum :encoding encoding :length length :content-type content-type)))))
;; #+END_SRC


;; Determine the checksum of the local file.  This is needed to see if it
;; is necessary to perform any transfers.
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-attachment-local-checksum (name)
  "Return the checksum for an attachment named NAME.  Requires
the program md5sum in the path.  If the file cannot be found,
return nil."
  (let ((file (org-attach-expand name)))
    (when (file-exists-p file)
     (org-couchdb-md5sum file))))
;; #+END_SRC

;; Iterate over all local attachment names.
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-map-local-attachments (fun)
  "Map FUN over org attachments at point."
  (let ((dir (org-attach-dir)))
    (when dir
      (mapcar fun
       (org-attach-file-list dir)))))


;; #+END_SRC


;; Comparison operation of attachment.  Used to determine which side
;; (local or remote) is present, and wether the checksums match.  Checks
;; the server for the corresponding attachment.
;; #+BEGIN_SRC emacs-lisp
(defun org-couchdb-check-attachment (attname)
  "Test if local attachment of entry at point is present on server and uptodate.  Returns:

:local if the attachment is present only locally and not on the server
:remote if the attachment is present only on the server and not locally
:missing if the attachment is neither present locally nor on the server
:match if the attachment is present remote and locally and the checksums match
:mismatch if the attachment is present remote and locally but the checksums don't match"
  (let ((id (org-entry-get nil "COUCHDB-ID")))
    (if id
      (catch 'result
	(let* ((info (or (org-couchdb-attachment-info (org-couchdb-db nil) id attname)))
	       (remote-md5 (plist-get info :md5sum))
	       (local-md5 (org-couchdb-attachment-local-checksum attname)))
	  ;; (message "debug: local-md5: %s remote-md5 %s" local-md5 remote-md5)
	  (cond (remote-md5 (if local-md5
				  (if (string-equal local-md5 remote-md5) (throw 'result :match)
				    (throw 'result :mismatch))
				(throw 'result :remote)))
		(local-md5 (if remote-md5
			       (if (string-equal local-md5 remote-md5) (throw 'result :match)
				 (throw 'result :mismatch))
			     (throw 'result :local)))
		(t (throw 'result :missing)))))
      (error "could not determine couchdb id"))))

;; #+END_SRC

;; *** TODO Bulk Processing
;;  This section deals with commands that process more than one item.
;;  Currently, following functionality is to be supported:
;;  1. Checking in all items in a buffer based on a org-mode tag/property query
;;  2. Updating all existing items in a buffer
;;  3. Creating a new subtree containing several items based on a query to
;;     CouchDB
;; ****  Checking in several items
;;  This uses org-map-entries with a query (which is prompted), to map the
;;  check-in function over all items.

;; #+BEGIN_SRC emacs-lisp
;; HACK: uses form copied from `org-make-tags-matcher' in order to create the query
(defun org-couchdb-store-all-entries (match)
  "Map over items designated by MATCH, performing a
   `org-couchdb-store-entry' on each."
  (interactive (list (completing-read
 		      "Match: "
 		      'org-tags-completion-function nil nil nil 'org-tags-history)))
  (org-map-entries 'org-couchdb-store-entry match))
;; #+END_SRC

;;; Footer:
;; #+BEGIN_SRC emacs-lisp

(provide 'org-couchdb)
;;; org-couchdb.el ends here
;; #+END_SRC
