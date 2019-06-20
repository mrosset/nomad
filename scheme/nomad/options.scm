;; Options.scm --- command line options for nomad
;; Copyright (C) 2019 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad options)
  #:use-module (nomad browser)
  #:use-module (ice-9 getopt-long)
  #:export (display-options
	    option-client
	    option-listen
	    option-url))

(define option-spec
  '((listen  (value #t))
    (client  (single-char #\c) (value #f))
    (version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))

(define (get-option key options default)
  (option-ref (getopt-long options option-spec) key default))

(define (option-client options)
  (get-option 'client options #f))

(define (option-listen options)
  (get-option 'listen options "/tmp/nomad-socket"))

(define (option-url options)
  (let ((url (get-option '() options default-home-page)))
    (if (null? url)
        default-home-page
        (car url))))
