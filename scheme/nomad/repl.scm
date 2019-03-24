;; repl.scm
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

(define-module (nomad repl)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 threads)
  #:use-module (nomad app)
  #:use-module (nomad options)
  #:use-module (rnrs bytevectors)
  #:use-module (system repl coop-server)
  #:use-module (system repl server)
  #:export (
	    emacs-command-line
	    nc-command-line
	    rlwrap-command-line
	    repl-command-line
	    repl-server
	    socket-exists?
	    server-force-delete
	    server-start
	    write-socket
	    server-start-coop
	    client-start))

(define emacs-command-line (list "emacs" "-q" "-nw" "-l" emacs-init-file))

;; (define nc-command-line (list "nc" "-U" socket-file))

;; (define rlwrap-command-line (append (list "rlwrap") nc-command-line))

;; (define repl-command-line nc-command-line)

(define repl-server #f)

(define (poll-server)
  (poll-coop-repl-server repl-server)
  (usleep 100000)
  (poll-server))

(define (server-start-coop socket-file)
  (when (file-exists? socket-file)
    (delete-file socket-file))
  (set! repl-server
        (spawn-coop-repl-server (make-unix-domain-server-socket #:path socket-file)))
  (make-thread (poll-server)))

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

(define (read-socket port)
  (do ((line (read-line port) (read-line port)))
      ((eof-object? line))
    (display line)
    (newline)))

(define client-port #f)

(define (read-to-end port)
  (display "flushing port\n")
  (do ((line (read-line port) (read-line port)))
      ((string=? line "Enter `,help' for help."))
      ;; ((eof-object? line))
    (display line)
    (newline)))

(define (client-start socket-file)
  (when (not (access? socket-file W_OK))
    (display "socket is not readable")
    (exit))
  (set! client-port (socket PF_UNIX SOCK_STREAM 0))
  (connect client-port AF_UNIX socket-file)
  (read-to-end client-port)
  (activate-readline)
  (set-readline-prompt! "> ")
  (set-readline-output-port! client-port)
    (do ((line (readline) (readline)))
	((string=? line "exit"))
      (write-line line client-port)
      (display (read-line client-port))
      (newline)))

(define (write-socket input socket-file)
  (let ((port (socket PF_UNIX SOCK_STREAM 0)))
     (catch #t
      (lambda ()
	       (connect port AF_UNIX socket-file)
	       (write-line input port))
      (lambda (key . parameters)
	(format #t "~s: ~s ~s" key parameters socket-file)))))
