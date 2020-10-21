;; doc.scm
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

(define-module (tests doc)
  #:use-module (oop goops)
  #:use-module (nomad doc)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (unit-test))

(define-class <test-doc> (<test-case>))

(define test-file "tests/doc-test.scm")

(define scheme-path (canonicalize-path (string-append (dirname (getcwd)) "/scheme")))

(define-method (test-doc-class (self <test-doc>))
  (let ((doc      (find-doc 'eval))
        (kb       (find-doc 'kill-buffer))
        (web      (find-doc 'web-mode)))
    (assert-true  doc)
    (assert-true kb)
    (assert-equal '(guile) (module-name (!module doc)))
    (assert-equal "(guile)" (module->string doc))
    (assert-equal "eval" (!name doc))
    (assert-equal 409 (string-length (!docstring doc)))
    (assert-equal (string-append scheme-path // "nomad/web.scm") (filename web))
    (assert-equal (string-append "nomad://file" scheme-path // "nomad/web.scm")
                  (module-uri web))
    (assert-equal "nomad:not-found" (module-uri doc))))

(define-method (test-doc-loc (self <test-doc>))
  (let ((get-line (lambda (sym)
                    (assoc-ref (doc-loc test-file sym) 'line))))
    (assert-equal 21 (get-line '<first-class>))
    (assert-equal 23 (get-line 'first-proc))
    (assert-equal 25 (get-line 'second-proc))
    (assert-equal 27 (get-line 'optional-proc))
    (assert-equal 32 (get-line 'first-binding))
    (assert-equal 34 (get-line 'second-binding))
    (assert-false (get-line 'no-symbol))))

(define-method (test-doc-procs (self <test-doc>))
  (assert-equal (string-append scheme-path "/nomad/nomad.scm")
                (module-filename '(nomad nomad))))

(define-method (test-doc-module (self <test-doc>))
  (assert-false (doc-loc (module-filename '(nomad web)) '<web-buffer>)))
