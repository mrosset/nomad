;; application.scm
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

(define-module (tests application)
  #:use-module (oop goops)
  #:use-module (nomad application)
  #:use-module (nomad gtk application)
  #:use-module (nomad util)
  #:use-module (nomad init)
  #:use-module (g-golf)
  #:use-module (unit-test)
  #:export (with-test-app
            make-test-app
            g-inst
            %test-app-id))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (gi-import "Gio")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "ApplicationWindow")
              ("Gtk" . "Widget"))))

(define %test-app-id "org.gnu.test.nomad")

(define (g-inst widget)
  (!g-inst widget))

(define (make-test-app)
  (make <gtk-application> #:application-id %test-app-id))

(define-syntax with-test-app
  (syntax-rules ()
    ((with-test-app app body)
     (let ((activate (lambda (app)
              body)))
       (connect app 'activate activate)
       (g-application-run app 0 #f)))))

(define-class <test-application> (<test-case>))

(define-method (test-with-application (self <test-application>))
  (let ((app (make-test-app)))
    (with-test-app app
                   (assert-equal %test-app-id
                                 (g-application-get-application-id app)))))

;; (define-method (test-application-id (self <test-application>))
;;   (with-fluids ((~ "/tmp/home"))
;;     (assert-equal "/tmp/home/.nomad" (user-init-file))
;;     ;; (let* ((test-id "org.gnu.tests.nomad")
;;     ;;        (app (make <nomad-gtk-application> #:application-id test-id)))
;;     ;;   (assert-equal test-id (application-id)))
;;     ))
