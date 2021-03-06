;; eval.scm
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

(define-module (nomad eval)
  #:use-module (ice-9 session)
  #:export (input-eval))

(define (input-eval input)
  (let ((result #nil)
         (error #nil))
    (catch #t
      (lambda ()
        (set! result (format #f "~a" (eval-string input))))
      (lambda (key . parameters)
            (set! error (format #f "Uncaught throw to '~a: ~a\n" key parameters))))
    (values result error)))
