;; util.scm
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

(define-module (tests util)
  #:use-module (oop goops)
  #:use-module (nomad util)
  #:use-module (unit-test))

(define-class <test-util> (<test-case>))

(define-method (test-fluids (self <test-util>))
  (with-fluids ((fluid~ "/tmp/home"))
    (assert-equal "/tmp/home" ~)
    (assert-equal "/tmp/home/file" (~/ "file"))))

(define-method (test-info (self <test-util>))
  (let ((proc (lambda _
                (info "test"))))
    (assert-equal "INFO: test\n" (with-output-to-string proc))))

(define-method (test-ensure-directory (self <test-util>))
  (let ((dir (tmpnam)))
    (dynamic-wind
      (lambda _
        (ensure-directory dir))
      (lambda _
        (assert-true (file-exists? dir)))
      (lambda _
        (rmdir dir)
        (assert-false (file-exists? dir))))))
