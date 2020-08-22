;; repl.scm
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

(define-module (nomad repl)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 threads)
  #:use-module (nomad application)
  #:use-module (nomad options)
  #:use-module (nomad platform)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 textual-ports)
  #:use-module (system repl coop-server)
  #:use-module (system repl server)
  #:export (repl-server
            server-force-delete
            server-start
            server-start-coop
            socket-file))

(define socket-file "/tmp/nomad-socket")

(define repl-server #f)

(define (poll-server)
  (poll-coop-repl-server repl-server)
  (usleep 50000)
  (poll-server))

(define (server-start-coop socket-file)
  (when (file-exists? socket-file)
    (delete-file socket-file))
  (set! repl-server
        (spawn-coop-repl-server (make-unix-domain-server-socket #:path socket-file)))
  (timeout-add 50
               (lambda _
                 (poll-coop-repl-server repl-server)
                 #t)))

(define (server-start socket-file)
  "Spawn a UNIX domain sockert REPL in a new thread. The file is the
value of socket-file."
  (when (file-exists? socket-file)
    (delete-file socket-file))
    (spawn-server
     (make-unix-domain-server-socket #:path socket-file)))

(define (socket-exists? socket-file)
  (access? socket-file R_OK))

;; FIXME: if socket clients are connected, (server-force-delete) will
;; throw an excpetion. Which gives us a truncated Backtrace to
;; stderr. More then likely the end user wants to kill connected
;; clients. Maybe we should prompt the user? Either way we should
;; handle the exception in a cleaner way.
(define (server-force-delete socket-file)
  "Unconditionally delete connection file. Stops REPL server and
client connections first."
  (stop-server-and-clients!)
  (delete-file socket-file))

(define client-port #f)

(define (read-until-prompt port)
  "Read from PORT until prompt has been read or the end-of-file was
reached."
  (while #t
    (let ((c (get-char port)))
      (when (eof-object? c)
        (break))
      ;; if char is > and there is a space after, assume this is the
      ;; prompt and stop
      (when (and (char=? c #\>) (char=? (lookahead-char port) #\space))
        (get-char port)
        (break))
      (display c))))

(define (client-start path)
  "Starts a client connected to a guile unix socket REPL server"
  (when (not (access? path W_OK))
    (display "socket is not readable")
    (exit))
  ;; setup readline
  (activate-readline)
  (set-readline-prompt! "> ")
  ;; create port and enter readline loop
  (let ((port (socket PF_UNIX SOCK_STREAM 0)))
    (connect port AF_UNIX path)
    (read-until-prompt port)
    (newline)
    (while #t
      (let ((line (readline)))
        (write-line line port)
        (read-until-prompt port)
        (newline)))))

(define-public (write-socket input socket-file)
  "Write string INPUT to guile unix socket at SOCKET-FILE. The guile
instance on the socket will evaluate INPUT expression. It is not
possible to return anything from the socket at this point"
  (let ((port (socket PF_UNIX SOCK_STREAM 0)))
    (catch #t
      (lambda ()
        (connect port AF_UNIX socket-file)
        (write-line input port))
      (lambda (key . parameters)
        (format #t "~s: ~s ~s" key parameters socket-file)))))
