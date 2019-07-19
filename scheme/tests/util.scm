;; util.scm
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

(define-module (tests util)
  #:use-module (nomad util)
  #:use-module (srfi srfi-64))

(test-begin "util")
(with-fluids ((~ "/tmp/home"))
  (test-equal "procedure ~  expands to $HOME" (fluid-ref ~) "/tmp/home")
  (test-equal "procedure ~/ expands to $HOME/child" (~/ "test") "/tmp/home/test"))
(with-fluids ((log-info? #t))
  (test-equal "info procedure"
    (format #f "INFO: test\n")
    (with-output-to-string (lambda _
                             (info "test")))))
(test-end)
