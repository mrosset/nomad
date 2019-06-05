;; buffer.scm
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

(define-module (nomad buffer)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (nomad app)
  #:use-module (nomad repl)
  #:use-module (nomad eval))

(define-public (buffer-with-id key)
  "Returns a buffer from the buffer alist with ID. If a buffer with ID
is not found returns #f"
  (let* ((result #f) (pair (assv key (buffer-alist))))
    (when pair
      (set! result (cdr pair)))
    result))

(define-public (make-buffer-socket url socket)
  (let ((exp (format #f "(make-buffer \"~a\")" url)))
    (write-socket exp socket)))

(define-command (kill-some-buffers)
  "Kill all buffers but one"
  (for-each (lambda (arg)
              (kill-buffer)) (buffer-alist)))

(define (format-buffer buffer)
  "Returns a human readable buffer string in 80 column format"
  ;; (format #f "id: ~80:@y title: ~80:@y uri: ~80:@y\n"
  (format #f "~a\t ~a"
          (car buffer)
          (buffer-uri (cdr buffer))))

(define-public (buffers)
  (with-output-to-string pp-buffers))

(define-public (pp-buffers)
  "Pretty prints buffers-alist."
  (for-each (lambda (x)
              (format #t "~a\n" (format-buffer x))) (buffer-alist)))
