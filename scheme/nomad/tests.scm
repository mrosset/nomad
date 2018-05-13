;; tests.scm
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

(define-module (nomad tests)
  #:use-module (nomad browser)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 pretty-print)
  #:export (run-tests))

(define (run-tests)
  (format #t "~a\n" (print-buffers)))

(define (run-tests-long)
  (next-buffer)
  (sleep 2)
  (browse "https://www.gnu.org/s/guile")
  (sleep 2)
  (next-buffer)
  (sleep 2)
  (next-buffer)
  (pretty-print (all-threads))
  (current-thread))
