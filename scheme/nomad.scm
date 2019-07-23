#!/home/nly/.guix-profile/bin/guile -s
!#

;; Nomad --- Extensible web browser

;; Copyright (C) 2019  Amar Singh

;; This file is part of Nomad.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: support Emacs-style command line flags. Flags processed in scheme.

(define-module (nomad)
  #:use-module (ice-9 getopt-long))

;;; options
(define option-spec
       '((version (single-char #\v) (value #f))
         (help (single-char #\h) (value #f))
         (listen (single-char #\L) (value #f))
         (client (single-char #\c) (value #f))
         (app-id (single-char #\I) (value #f))))

(define options (getopt-long (command-line) option-spec))

(define scm (current-filename))

(define src (dirname (dirname scm)))

(define (src/ str) (string-append src "/" str))

(define* (main #:optional args)
  (let ((help (option-ref options 'help #f))
        (version (option-ref options 'version #f)))
    (cond
     (version (display "0.9\n"))
     (help (display "This is alpha software; there is no help. You're
welcome!\n"))
     (#t
      (begin
        (system (string-join (list (src/ "src/nomad") "-c '(boot-nomad)'")
                             " "))
        ;; test
        (display (command-line))
        (newline))))))

(main)
