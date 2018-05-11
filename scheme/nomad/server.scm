;; server.scm
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

(define-module (nomad server))

(define client-socket (socket PF_UNIX SOCK_STREAM 0))
(define socket-path "/tmp/nomad")

(define (message msg)
  (connect client-socket AF_UNIX socket-path)
  (simple-format client-socket "~a\n" msg)
  (close-port client-socket))

(define server-socket #f)

(define (eval-line client-port)
  (let ((line (read-line client-port)))
    (false-if-exception (eval-string line))))

;; (define (run-server)
;;   (when (file-exists? socket-path)
;;     (delete-file socket-path))

;;   (set! server-socket (make-unix-domain-server-socket #:path socket-path))

;;   (sigaction SIGPIPE SIG_IGN)
;;   (listen server-socket 1)
;;   (let loop ()
;;     (match (accept server-socket)
;;       (#f
;;        (close server-socket))
;;       ((client-port . client-addr)
;;        (eval-line client-port)
;;        (close-port client-port)
;;        (loop)))))
