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

(define-module (tests buffer)
  #:use-module (emacsy emacsy)
  #:use-module (nomad app)
  #:use-module (nomad buffer)
  #:use-module (nomad webview)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))


(test-begin "buffers")

(unless emacsy-interactive?
  (emacsy-initialize #t))

(test-assert "interactive?" emacsy-interactive?)
(test-equal "scratch and messages?" 2 (length (buffer-list)))

(test-equal "don't switch on current"
  #f
  (let ((buffer (switch-to-buffer "*bar*")))
    (switch-if-not-current buffer)))

(test-assert "switch when not current"
  (let ((buffer (switch-to-buffer "*bar*")))
    (switch-to-buffer "*baz*")
    (switch-if-not-current buffer)))
