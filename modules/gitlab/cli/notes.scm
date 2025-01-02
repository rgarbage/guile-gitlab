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

;; This module contains the implementation of 'gitlab-cli notes' command that can
;; be used to work with GitLab Notes API (comments in Gitlab terminology).


;;; Code:

(define-module (gitlab cli notes)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (gitlab)
  #:use-module (gitlab cli common)
  #:use-module (gitlab api notes)
  #:export (gitlab-cli-notes))

(define (print-notes-help program-name)
  (format #t "\
Usage: ~a notes <sub-command> [arguments]

Available subcommands:
  create, c   Create a new note.
  edit, e     Edit the specified note.
  list, ls    List notes.
  delete, d   Delete the specified note.

Common options:
  --token <token>
              API token to use for authentication (mandatory).
  --server <server>
              Gitlab server to connect to (mandatory).
  --limit <limit>
              Limit the number of notes that will be requested
              from the server.  By default, the command will fetch
              all the notes that satisfy the requirements set by
              other options.
  --project-id <id>
              ID of the parent project of the merge request, issue
              or snippet containing the note (mandatory).
  --merge-request-iid <iid>
              IID of the merge request containing the notes.
  --issue-iid <iid>
              IID of the issue containing the notes.
  --snippet-id <id>
              ID of the snippet containing the notes.

Subcommand-specific options:
  --note-id <id>
              ID of the note to act on [edit, delete] (mandatory).
  --body <string>
              Note content [edit, create] (mandatory).
  --order-by <criteria>
              Ordering criteria [list]. Valid values are \"updated_at\"
              and \"created_at\".
  --sort <criteria>
              Sorting order [list]. Valid values are \"asc\" and
              \"desc\".
"
          program-name))

(define %user-option-spec
  '((help   (single-char #\h) (value #f))
    (server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (limit  (single-char #\l) (value #t))
    (print  (single-char #\p) (value #t))
    (format (single-char #\f) (value #t))
    (merge-request-iid        (value #t))
    (snippet-id               (value #t))
    (issue-iid                (value #t))
    (note-id                  (value #t))
    (project-id               (value #t))
    (body                     (value #t))
    (active?                  (value #t))
    (order-by                 (value #t))
    (sort                     (value #t))))

(define (gitlab-cli-notes-list program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
         ;; Required parameters.
         (server            (option-ref options 'server            #f))
         (token             (option-ref options 'token             #f))
         (merge-request-iid (option-ref options 'merge-request-iid #f))
         (issue-iid         (option-ref options 'issue-iid         #f))
         (snippet-id        (option-ref options 'snippet-id        #f))
         (project-id        (option-ref options 'project-id        #f))
         ;; Optional parameters.
         (limit             (option-ref options 'limit             #f))
         (active?           (option-ref options 'active?           'undefined))
         (order-by          (option-ref options 'order-by          'undefined))
         (sort              (option-ref options 'sort              'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-notes-help program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (unless project-id
      (error "'--project-id' option must be specified" args))

    (unless (or merge-request-iid
                snippet-id
                issue-iid)
      (error "either '--merge-request-iid', '--snippet-id' or '--issue-iid' option must be specified" args))

    (when (> (length (filter ->bool
                             (list merge-request-iid
                                   snippet-id
                                   issue-iid)))
             1)
      (error "options '--merge-request-iid', '--snippet-id' and '--issue-iid' are mutually exclusive" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (cond
                    (merge-request-iid
                     (gitlab-api-notes-list session
                                            project-id
                                            #:object-id merge-request-iid
                                            #:object-type 'merge-request))
                    (issue-iid
                     (gitlab-api-notes-list session
                                            project-id
                                            #:object-id issue-iid
                                            #:object-type 'issue))
                    (snippet-id
                     (gitlab-api-notes-list session
                                            project-id
                                            #:object-id snippet-id
                                            #:object-type 'snippet))
                    (else
                     ;; This case should never happen
                     #f))))
      (print (vector->list result)  fields #:format print-format))))

(define (gitlab-cli-notes-edit program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
         ;; Required parameters.
         (server            (option-ref options 'server            #f))
         (token             (option-ref options 'token             #f))
         (merge-request-iid (option-ref options 'merge-request-iid #f))
         (issue-iid         (option-ref options 'issue-iid         #f))
         (snippet-id        (option-ref options 'snippet-id        #f))
         (project-id        (option-ref options 'project-id        #f))
         (note-id           (option-ref options 'note-id           #f))
         (body              (option-ref options 'body              #f))
         ;; Optional parameters.
         (limit             (option-ref options 'limit             #f))
         (active?           (option-ref options 'active?           'undefined))
         (order-by          (option-ref options 'order-by          'undefined))
         (sort              (option-ref options 'sort              'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-notes-help/list program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (unless project-id
      (error "'--project-id' option must be specified" args))

    (unless (or merge-request-iid
                snippet-id
                issue-iid)
      (error "either '--merge-request-iid', '--snippet-id' or '--issue-iid' option must be specified" args))

    (when (> (length (filter ->bool
                             (list merge-request-iid
                                   snippet-id
                                   issue-iid)))
             1)
      (error "options '--merge-request-iid', '--snippet-id' and '--issue-iid' are mutually exclusive" args))

    (unless note-id
      (error "'--note-id' option must be specified" args))

    (unless body
      (error "'--body' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (cond
                    (merge-request-iid
                     (gitlab-api-notes-edit session
                                            project-id
                                            note-id
                                            body
                                            #:object-id merge-request-iid
                                            #:object-type 'merge-request))
                    (issue-iid
                     (gitlab-api-notes-edit session
                                            project-id
                                            note-id
                                            body
                                            #:object-id issue-iid
                                            #:object-type 'issue))
                    (snippet-id
                     (gitlab-api-notes-edit session
                                            project-id
                                            note-id
                                            body
                                            #:object-id snippet-id
                                            #:object-type 'snippet))
                    (else
                     ;; This case should never happen
                     #f))))
      (print result fields #:format print-format))))

(define (gitlab-cli-notes-delete program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
         ;; Required parameters.
         (server            (option-ref options 'server            #f))
         (token             (option-ref options 'token             #f))
         (merge-request-iid (option-ref options 'merge-request-iid #f))
         (issue-iid         (option-ref options 'issue-iid         #f))
         (snippet-id        (option-ref options 'snippet-id        #f))
         (project-id        (option-ref options 'project-id        #f))
         (note-id           (option-ref options 'note-id           #f)))

    (when (or help-needed? (< (length args) 2))
      (print-notes-help/list program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (unless project-id
      (error "'--project-id' option must be specified" args))

    (unless (or merge-request-iid
                snippet-id
                issue-iid)
      (error "either '--merge-request-iid', '--snippet-id' or '--issue-iid' option must be specified" args))

    (when (> (length (filter ->bool
                             (list merge-request-iid
                                   snippet-id
                                   issue-iid)))
             1)
      (error "options '--merge-request-iid', '--snippet-id' and '--issue-iid' are mutually exclusive" args))

    (unless note-id
      (error "'--note-id' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (cond
                    (merge-request-iid
                     (gitlab-api-notes-delete session
                                              project-id
                                              note-id
                                              #:object-id merge-request-iid
                                              #:object-type 'merge-request))
                    (issue-iid
                     (gitlab-api-notes-delete session
                                              project-id
                                              note-id
                                              #:object-id issue-iid
                                              #:object-type 'issue))
                    (snippet-id
                     (gitlab-api-notes-delete session
                                              project-id
                                              note-id
                                              #:object-id snippet-id
                                              #:object-type 'snippet))
                    (else
                     ;; This case should never happen
                     #f))))
      (print result fields #:format print-format))))

(define (gitlab-cli-notes-create program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
         ;; Required parameters.
         (server            (option-ref options 'server            #f))
         (token             (option-ref options 'token             #f))
         (merge-request-iid (option-ref options 'merge-request-iid #f))
         (issue-iid         (option-ref options 'issue-iid         #f))
         (snippet-id        (option-ref options 'snippet-id        #f))
         (project-id        (option-ref options 'project-id        #f))
         (body              (option-ref options 'body              #f))
         ;; Optional parameters.
         (limit             (option-ref options 'limit             #f))
         (active?           (option-ref options 'active?           'undefined))
         (order-by          (option-ref options 'order-by          'undefined))
         (sort              (option-ref options 'sort              'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-notes-help/list program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (unless project-id
      (error "'--project-id' option must be specified" args))

    (unless (or merge-request-iid
                snippet-id
                issue-iid)
      (error "either '--merge-request-iid', '--snippet-id' or '--issue-iid' option must be specified" args))

    (when (> (length (filter ->bool
                             (list merge-request-iid
                                   snippet-id
                                   issue-iid)))
             1)
      (error "options '--merge-request-iid', '--snippet-id' and '--issue-iid' are mutually exclusive" args))

    (unless body
      (error "'--body' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (cond
                    (merge-request-iid
                     (gitlab-api-notes-create session
                                              project-id
                                              body
                                              #:object-id merge-request-iid
                                              #:object-type 'merge-request))
                    (issue-iid
                     (gitlab-api-notes-create session
                                              project-id
                                              body
                                              #:object-id issue-iid
                                              #:object-type 'issue))
                    (snippet-id
                     (gitlab-api-notes-create session
                                              project-id
                                              body
                                              #:object-id snippet-id
                                              #:object-type 'snippet))
                    (else
                     ;; This case should never happen
                     #f))))
      (print result fields #:format print-format))))

(define %commands
  `((("list" "ls")        ,gitlab-cli-notes-list)
    (("edit" "e")         ,gitlab-cli-notes-edit)
    (("delete" "d")       ,gitlab-cli-notes-delete)
    (("create" "c")       ,gitlab-cli-notes-create)))

(define (gitlab-cli-notes program-name args)
  (when (zero? (length args))
    (print-notes-help program-name)
    (exit 0))

  (let* ((sub-command (car args))
         (handler     (command-match sub-command %commands)))
    (if handler
        (handler program-name (cdr args))
        (begin
          (print-notes-help program-name)
          (exit 0)))))

;;; notes.scm ends here.
