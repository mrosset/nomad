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
  #:use-module (nomad keymap)
  #:use-module (nomad events)
  #:use-module (nomad util)
  #:use-module (nomad eval)
  #:use-module (nomad buffer)
  #:use-module (nomad repl)
  #:use-module (nomad options)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (init
            user-cookie-file
            user-init-file
            user-init-hook
            user-nomad-directory
            create-nomad-directory))

(define user-init-hook (make-hook))

(define user-init-file
  (~/ ".nomad"))

(define user-nomad-directory
  (make-fluid (~/ ".nomad.d")))

(define-public history '())

(define-public history-file
  (make-fluid (string-append (fluid-ref user-nomad-directory)
                             //
                             "history.scm")))

(define-public (read-history)
  (let ((port (open-input-file (fluid-ref history-file))))
    (set! history (read port))
    (close-port port)))

(define-public (nomad-write-history)
  (let ((port (open-output-file (fluid-ref history-file))))
    (pretty-print history port)
    (close-port port)))

(define-public (add-to-history text)
  (when (not (find (lambda (x)
                     (string=? x text))
                   history))
    (set! history
      (append history
              (list text)))))

(add-hook! command-hook (lambda (arg)
                          (add-to-history arg)))
(define session '())

(define-public session-file
  (make-fluid (string-append (fluid-ref user-nomad-directory)
                             //
                             "session.scm")))

(define-command (read-session)
  "Read session from file"
  (let* ((port (open-input-file (fluid-ref session-file)))
         (buffers (read port)))
    (close-port port)
    (for-each (lambda (url) (make-buffer url)) buffers)))

(define-command (write-session)
  "Write session to file"
  (let* ((port (open-output-file (fluid-ref session-file)))
         (buffers (buffers->list)))
    (pretty-print buffers port)
    (close-port port)))

(define user-cookie-file
  (string-append (fluid-ref user-nomad-directory) // "cookies.db"))

(define (create-nomad-directory)
  (let ((dir (fluid-ref user-nomad-directory)))
    (when (not (file-exists? dir))
      (info (format #f "creating ~a" dir))
      (mkdir dir #o755))))

(define-public (shutdown)
  "Cleans up after guile and serialize persistent objects"
  (format #t "writing history....\n")
  (nomad-write-history)
  (format #t "shutting down repl...\n")
  (server-force-delete (option-listen (command-line))))

(define (init)
  (add-hook! key-press-hook handle-key-press)
  (add-hook! event-hook debug-event)
  (create-nomad-directory)
  (when (file-exists? (fluid-ref history-file))
    (read-history))
  (when (file-exists? user-init-file)
    (load user-init-file)))
