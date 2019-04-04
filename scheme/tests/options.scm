;; options.scm
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

(define-module (tests options)
  #:use-module (nomad options)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-64))

(define test-command-line '("./nomad" "https://gnu.org" "--listen" "/tmp/test"))
(define test-arg0 '("./nomad"))
(define test-client '("./nomad" "-c"))

(test-begin "options")

(test-equal "option listen" (option-listen test-command-line) "/tmp/test")

(test-equal "option url"
  (option-url test-command-line) "https://gnu.org")

(test-assert "option client" (option-client test-client))

(test-assert"option no client" (not (option-client test-arg0)))

(test-equal "no listen" (option-listen test-arg0) "/tmp/nomad-socket")

(test-equal "no url" (option-url test-arg0) "https://www.gnu.org/software/guile")

(test-end)
