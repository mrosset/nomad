;; frame.scm
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

(define-module (tests frame)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (nomad frame)
  #:use-module (nomad gtk frame)
  #:use-module (unit-test))

(eval-when (expand load eval)
  (map (lambda (item)
         (gi-import-by-name (car item) (cdr item)))
       '(("Gtk" . "init_check")
         ("Gtk" . "Application")
         ("Gtk" . "Notebook"))))

(define-class <test-frame> (<test-case>))

(define-method (test-frame (self <test-frame>))
  (let* ((app (make <gtk-application> #:application-id "org.gnu.test.nomad"))
         (activate (lambda (app)
                     (let* ((frame    (gtk-frame-new app)))
                       (assert-equal <gtk-frame> (class-of frame))
                       (assert-equal <gtk-frame> (class-of (current-frame)))
                       (g-application-quit app)))))
    (connect app 'activate activate)
    (g-application-run app 0 #f)))
