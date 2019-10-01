;; qt.scm
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

(define-module (nomad qt)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<qt-main-window>
            make-qt-main-window))

(gi-import "Qt")
(for-each (lambda (x)
            (gi-import-by-name "Gtk" x))
          '("Widget" "Container"))

(define-class <qt-main-window> (<qt-widget>))

(define (make-qt-main-window)
  (let ((window (make <qt-main-window>))
        (view (make <qt-web-view>))
        (label (make <qt-label> #:label "hello")))
    (qt-web-view-load-uri view "http://google.ca")
    (gtk-container-add window view)
    (gtk-container-add window label)
    window))
