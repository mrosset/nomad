;; web.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (tests web)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad web)
  #:use-module (unit-test))

(define-class <test-web> (<test-case>))

(define-class <test-buffer> (<web-buffer>))

(define-method (test-class (test <test-web>))
  (let ((buffer (make <test-buffer> #:uri "https://gnu.org")))
    (assert-equal "https://gnu.org" (buffer-uri buffer))))
