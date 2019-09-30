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
(define test-client '("./nomad" "--gapplication-app-id" "org.devel.nomad" "-c"))
(define test-quick '("./nomad" "-Q"))
(define test-platform '("./nomad" "--platform" "qt"))

(test-begin "options")

(test-equal "gtk" (option-platform test-arg0))
(test-equal "qt" (option-platform test-platform))

(test-equal "/tmp/test" (option-listen test-command-line))
(test-equal "/tmp/nomad-socket" (option-listen test-arg0))

(test-equal "option url" "https://gnu.org" (option-url test-command-line))

(test-equal "org.gnu.nomad" (option-app-id test-arg0))

(test-equal "org.devel.nomad" (option-app-id test-client))

(test-assert "option client" (option-client test-client))

(test-assert"option no client" (not (option-client test-arg0)))

(test-equal "no listen" "/tmp/nomad-socket" (option-listen test-arg0))

(test-equal "no url" "https://www.gnu.org/software/guile" (option-url test-arg0))

(test-assert "quick" (option-quick test-quick))

(test-end)
