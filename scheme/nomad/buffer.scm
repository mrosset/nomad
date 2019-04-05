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
  #:use-module (nomad eval)
  #:export (pp-buffers
            kill-buffers
            make-buffer-socket))

(define (make-buffer-socket url socket)
  (let ((exp (format #f "(make-buffer \"~a\")" url)))
    (write-socket exp socket)))

(define-command (kill-some-buffers)
  "Kill all buffers but one"
  (for-each (lambda (arg)
              (kill-buffer)) (buffer-alist)))

(define (format-buffer buffer)
  "Returns a human readable buffer string in 80 column format"
  (format #f "id: ~80:@y uri: ~80:@y\n"
          (car buffer)
          (cdr buffer)))

(define (pp-buffers)
  "Pretty prints buffers-alist."
  (for-each (lambda (x)
              (format #t "~a" (format-buffer x))) (buffer-alist)))
