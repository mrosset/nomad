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
  #:use-module (nomad util)
  #:use-module (emacsy emacsy)
  #:use-module (nomad eval)
  #:use-module (nomad buffer)
  #:use-module (nomad webview)
  #:use-module (nomad repl)
  #:use-module (nomad options)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (init
            use-cookies?
            user-cookie-file
            %user-init-file
            %user-nomad-directory
            startup-hook
            download-directory
            define-ident
            session-file))

(define startup-hook (make-hook))

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

;; Path of user's initialization file. This is a top level identifier, it's
;; default value is @code{$HOME/.nomad}.
(define-ident %user-init-file
   (~/ ".nomad"))

;; Directory where per-user configuration files are placed. It's default value
;; is @code{$HOME/.nomad.d}.
(define-ident %user-nomad-directory
  (~/ ".nomad.d"))

(define-ident %download-directory
  (~/ "downloads"))

(define session '())

(define session-file
  (make-fluid (string-append %user-nomad-directory
                             //
                             "session.scm")))

(define-interactive (read-session)
  "Read session from file"
  (let* ((port (open-input-file (fluid-ref session-file)))
         (buffers (read port)))
    (close-port port)
    (for-each (lambda (uri)
                (when (not (buffers-contain? uri))
                  (make-buffer uri)))
              buffers)))

(define-interactive (write-session)
  "Write session to file"
  (let* ((port (open-output-file (fluid-ref session-file)))
         (buffers (buffers->uri)))
    (pretty-print buffers port)
    (close-port port)))

(define use-cookies? #t)

(define user-cookie-file
  (string-append %user-nomad-directory // "cookies.db"))

(define (init)
  (ensure-directory %user-nomad-directory)
  ;; If user-init-file exists and -Q is not passed as a command line argument
  ;; then load the user-init-file
  (when (and (not (option-quick (command-line)))
             (file-exists? %user-init-file))
    (load %user-init-file))
  #t)
