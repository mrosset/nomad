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

(define-module (tests graphical)
  #:use-module (emacsy emacsy)
  #:use-module (nomad buffer)
  #:use-module (nomad webview)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-64))

(test-begin "graphical")

(test-equal "https://bufio.org/"
  (begin (make-buffer "bufio.org")
         (update-buffer-names)
         (switch-to-buffer "https://bufio.org/")
         (buffer-name (current-buffer))))

(test-equal "*scratch*"
  (begin (switch-to-buffer "*scratch*")
         (buffer-name (current-buffer))))

(test-end)
