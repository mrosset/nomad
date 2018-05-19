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
  #:use-module (srfi srfi-9)
  #:export (buffer-with-id pp-buffers))

(define (buffer-with-id key)
  "Returns a buffer from the buffer alist with ID. If a buffer with ID
is not found returns #f"
  (let* ((result #f) (pair (assv key (buffer-alist))))
    (when pair
      (set! result (cdr pair)))
    result))

(define (format-buffer buffer)
  "Returns a human readable buffer string in 80 column format"
  (format #f "id: ~80:@y title: ~80:@y uri: ~80:@y\n"
          (car buffer)
          (buffer-title (cdr buffer))
          (buffer-uri (cdr buffer))))

(define (pp-buffers)
  "Pretty prints buffers-alist."
  (for-each (lambda (x)
              (format #t "~a" (format-buffer x))) (buffer-alist)))
