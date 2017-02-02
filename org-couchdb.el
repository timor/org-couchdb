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


;;; Helper: determine property by either getting it from subtree, buffer or prompt

(defun org-couchdb-get-property (pom name &optional postprocessor)
  "Determine org property NAME at POM, ask user if not found.
Apply POSTPROCESSOR on the read value."
  (let ((p (or (org-entry-get pom name t)
	       (completing-read (format "Provide value for Property '%s': " name) ()))))
    (if postprocessor
	(funcall postprocessor p)
      p)))

(defun org-couchdb-server ()
  "Determine the server and port to use")

(defun org-couchdb-port ())

(defun org-couchdb-post-subtree ())



(provide 'org-couchdb)
;;; org-couchdb.el ends here
