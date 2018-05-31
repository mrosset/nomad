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
  #:use-module (system repl server)
  #:use-module (system repl coop-server)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 rdelim)
  #:use-module (nomad app)
  #:export (
            emacs-command-line
            nc-command-line
            rlwrap-command-line
            repl-command-line
            repl-server
            server-force-delete
            server-start
            server-start-coop
            socket-file))

(define socket-file "/tmp/nomad-socket")

(define emacs-command-line (list "emacs" "-q" "-nw" "-l" emacs-init-file))

(define nc-command-line (list "nc" "-U" socket-file))

(define rlwrap-command-line (append (list "rlwrap") nc-command-line))

(define repl-command-line nc-command-line)

(define repl-server #f)

(define (poll-server)
  (poll-coop-repl-server repl-server)
  (usleep 100000)
  (poll-server))

(define (server-start-coop)
  (when (file-exists? socket-file)
    (delete-file socket-file))
  (set! repl-server
        (spawn-coop-repl-server (make-unix-domain-server-socket #:path socket-file)))
  (make-thread (poll-server)))

(define (server-start )
  "Spawn a UNIX domain sockert REPL in a new thread. The file is the
value of socket-file."
  (when (file-exists? socket-file)
    (delete-file socket-file))
    (spawn-server
     (make-unix-domain-server-socket #:path socket-file)))

;; FIXME: if socket clients are connected, (server-force-delete) will
;; throw an excpetion. Which gives us a truncated Backtrace to
;; stderr. More then likely the end user wants to kill connected
;; clients. Maybe we should prompt the user? Either way we should
;; handle the exception in a cleaner way.
(define (server-force-delete)
  "Unconditionally delete connection file. Stops REPL server and
client connections first."
  (stop-server-and-clients!)
  (delete-file socket-file))

(define (client-start)
  (let ((s (socket PF_UNIX SOCK_STREAM 0)))
    (connect s AF_UNIX socket-file)
    (do ((line (read-line s) (read-line s)))
        ((eof-object? line))
      (display line)
      (newline))))
