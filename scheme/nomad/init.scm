;; init.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

;; This file is part of Nomad

;; Nomad is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Nomad is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nomad init)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 pretty-print)
  #:use-module (nomad buffer)
  #:use-module (nomad options)
  #:use-module (nomad util)
  #:use-module (nomad log)
  #:use-module (nomad web)
  #:use-module (oop goops)
  #:declarative? #f
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (init
            load-environment
            %use-cookies?
            %user-cookie-file
            %user-init-file
            %init-file-name
            %environment-file
            %user-nomad-directory
            %module-directory
            %session-file
            define-ident))

;; Macro that defines a top level identifier
;; define-ident create a new top level identifier of @var{var} with the
;; value of @var{val}.
(define-syntax define-ident
  (syntax-rules ()
    ((define-ident var val)
     (begin
       (define-syntax var
         (identifier-syntax
          val))
       (export var)))))

;; %environment-file is the file that's loaded when (load-environment) is called. It is
;; used to extend nomad with an environment that might be required before the
;; graphical toolkit is used.
(define %environment-file
  (make-parameter (~/ ".nomad.d/environment.scm")))

(define (load-environment)
  "Loads @var{%environment-file}. "
  (let ((file (%environment-file)))
    (when (file-exists? file)
      (load file))))

;; %init-file-name is a top level parameter used to change the base file name of the
;; %user-init-file. it's default value is @code{.nomad}.
(define %init-file-name (make-parameter ".nomad"))

;; Path of user's initialization file. This is a top level identifier, it's
;; default value is @code{$HOME/.nomad}.
(define-ident %user-init-file (~/ (%init-file-name)))

;; Directory where per-user configuration files are placed. It's default value
;; is @code{$HOME/.nomad.d}.
(define-ident %user-nomad-directory
  (~/ ".nomad.d"))

(define-ident %module-directory
  (string-append %user-nomad-directory "/modules"))

(define session '())

;; File to store session too.
(define-ident %session-file
  (string-append %user-nomad-directory
                            //
                            "session.scm"))

(define-interactive (read-session)
  "Read session from file"
  (let* ((port (open-input-file %session-file))
         (buffers (read port)))
    (close-port port)
    (for-each (lambda (uri)
                (unless (buffers-contain? uri)
                  (make-buffer <web-buffer> #:uri uri)))
              buffers)))

(define-interactive (write-session)
  "Write session to file"
  (let* ((port (open-output-file %session-file))
         (urls (buffers->uri)))
    (pretty-print urls port)
    (close-port port)))

(define %use-cookies? #f)

(define-ident %user-cookie-file
  (string-append %user-nomad-directory // "cookies.db"))

(define (init)
  (emacsy-initialize #t)
  (set! emacsy-log? #f)
  (ensure-directory %user-nomad-directory)
  ;; If user-init-file exists and -Q is not passed as a command line argument
  ;; then load the %user-init-file
  (when (and (not (option-quick (command-line)))
             (file-exists? %user-init-file))
    (catch #t
      (lambda _
        (load %user-init-file))
      (lambda (key . vals)
        (safe-message "Error: key: ~a value: ~a" key vals)))))
