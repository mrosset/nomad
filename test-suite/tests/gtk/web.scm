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

(define-module (tests gtk web)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (nomad platform)
  #:use-module (nomad web)
  #:use-module (tests application)
  #:use-module (unit-test))

(define-class <test-gtk-web> (<test-case>))


(define-method (test-web-buffer (test <test-gtk-web>))
  (with-test-app
   (let ((buffer (make <web-buffer> #:uri "https://bufio.org")))
     (assert-equal <widget-web-view> (class-of (buffer-widget buffer)))
     (assert-equal "https://bufio.org/" (widget-uri buffer))
     (assert-equal (widget-uri (buffer-widget buffer))
                   (buffer-uri buffer)))))
