;; init.scm
;; Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

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
            user-init-file
            startup-hook
            user-nomad-directory
            download-directory
            ensure-nomad-directory
            ensure-download-directory
            history
            history-file
            read-history
            nomad-write-history
            add-to-history
            session-file))

(define startup-hook (make-hook))

(define user-init-file
  (~/ ".nomad"))

(define user-nomad-directory
  (make-fluid (~/ ".nomad.d")))

(define download-directory
  (make-fluid (~/ "downloads")))

(define history '())

(define history-file
  (make-fluid (string-append (fluid-ref user-nomad-directory)
                             //
                             "history.scm")))

(define (read-history)
  (let ((port (open-input-file (fluid-ref history-file))))
    (set! history (read port))
    (close-port port)))

(define (nomad-write-history)
  (let ((port (open-output-file (fluid-ref history-file))))
    (pretty-print history port)
    (close-port port)))

(define (add-to-history text)
  (when (not (find (lambda (x)
                     (string=? x text))
                   history))
    (set! history
      (append history
              (list text)))))

(define session '())

(define session-file
  (make-fluid (string-append (fluid-ref user-nomad-directory)
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
  (string-append (fluid-ref user-nomad-directory) // "cookies.db"))

(define (ensure-fluid-directory path)
  "Ensures fluid directory PATH is created"
  (let ((dir (fluid-ref path)))
    (when (not (file-exists? dir))
      (info (format #f "creating ~a" dir))
      (mkdir dir #o755))))

(define (ensure-nomad-directory)
  "Ensures creation of user-nomad-directory"
  (ensure-fluid-directory user-nomad-directory))

(define (ensure-download-directory)
  "Ensure creation of download-directory"
  (ensure-fluid-directory download-directory))

(define (init)
  (ensure-nomad-directory)
  ;; If user-init-file exists and -Q is not passed as a command line argument
  ;; then load the user-init-file
  (when (and (not (option-quick (command-line)))
             (file-exists? user-init-file))
    (load user-init-file))
  #t)
