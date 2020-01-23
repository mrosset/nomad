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
  #:use-module (unit-test))

(eval-when (expand load eval)
  (gi-import-by-name "Gtk" "init_check"))

(define-class <test-application> (<test-case>))

(define-method (test-init-gtk (self <test-application>))
  (assert-true (gtk-init-check #f #f)))

;; (define-method (test-application-id (self <test-application>))
;;   (with-fluids ((~ "/tmp/home"))
;;     (assert-equal "/tmp/home/.nomad" (user-init-file))
;;     ;; (let* ((test-id "org.gnu.tests.nomad")
;;     ;;        (app (make <nomad-gtk-application> #:application-id test-id)))
;;     ;;   (assert-equal test-id (application-id)))
;;     ))
