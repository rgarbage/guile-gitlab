;;; notes.scm -- Code for GitLab Notes API.

;; Copyright (C) 2025 Romain Garbage <romain.garbage@inria.fr>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains procedures to work with GitLab Notes API. It allows
;; working with Gitlab comments.


;;; Code:

(define-module (gitlab api notes)
  #:use-module (gitlab api common)
  #:use-module (gitlab client)
  #:use-module (gitlab common)
  #:use-module (gitlab session)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:export (gitlab-api-notes-list
            gitlab-api-notes-get
            gitlab-api-notes-create
            gitlab-api-notes-edit
            gitlab-api-notes-delete))

;; Helper function used to translate a type to the corresponding API string.
(define (object-type->endpoint object-type)
  (match object-type
    ('merge-request
     "merge_requests")
    ('issue
     "issues")
    ('snippet
     "snippets")))

(define* (gitlab-api-notes-list session
                                project-id
                                #:key
                                object-id
                                object-type
                                ;; Default values of the API
                                (sort "desc")
                                (order-by "created_at"))
  "Returns the list of comments in the object merge request corresponding to
MERGE-REQUEST-IID associated to the project with PROJECT-ID."
  (let ((query
         (make-sieved-list
          (cons-or-null 'order_by order-by)
          (cons-or-null 'sort sort))))
    (api-get session
             (format #f
                     "/api/v4/projects/~a/~a/~a/notes/"
                     project-id
                     (object-type->endpoint object-type)
                     object-id)
             #:query query)))

(define* (gitlab-api-notes-get session
                               project-id
                               note-id
                               #:key
                               object-id
                               object-type)
  "Returns the comment with the id NOTE-ID from the merge request
MERGE-REQUEST-IID associated to the project PROJECT-ID."
  (client-get (gitlab-session-client session)
              (format #f
                      "/api/v4/projects/~a/~a/~a/notes/~a"
                      project-id
                      (object-type->endpoint object-type)
                      object-id
                      note-id)))

(define* (gitlab-api-notes-create session
                                  project-id
                                  body
                                  #:key
                                  object-id
                                  object-type)
  "Creates a new note/comment in merge request MERGE-REQUEST-IID associated to
project PROJECT-ID with BODY, a string, as the content."
  (client-post-form (gitlab-session-client session)
                    (format #f
                            "/api/v4/projects/~a/~a/~a/notes/"
                            project-id
                            (object-type->endpoint object-type)
                            object-id)
                    #:query `((body . ,(uri-encode body)))))

(define* (gitlab-api-notes-edit session
                                project-id
                                note-id
                                body
                                #:key
                                object-id
                                object-type)
  "Sets the content of the note/comment NOTE-ID in merge request
MERGE-REQUEST-IID associated to project PROJECT-ID to BODY, a string."
  (client-put-form (gitlab-session-client session)
                   (format #f
                           "/api/v4/projects/~a/~a/~a/notes/~a"
                           project-id
                           (object-type->endpoint object-type)
                           object-id
                           note-id)
                   #:query `((body . ,body))))

(define* (gitlab-api-notes-delete session
                                  project-id
                                  note-id
                                  #:key
                                  object-id
                                  object-type)
  "Deletes the note with the id NOTE-ID from the merge request MERGE-REQUEST-IID
associated to the project PROJECT-ID."
  (client-delete (gitlab-session-client session)
                 (format #f
                         "/api/v4/projects/~a/~a/~a/notes/~a"
                         project-id
                         (object-type->endpoint object-type)
                         object-id
                         note-id)))

;;; notes.scm ends here.
