;; views.scm
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

(define-module (test views)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad views)
  #:use-module (nomad ibuffer)
  #:use-module (ice-9 regex)
  #:use-module (unit-test))

(define-class <test-views> (<test-case>))

(define command->proc-name (@@ (nomad views) command->proc-name))
(define test-map (make-keymap))

(define-interactive (test-command)
  #t)

(define-key test-map "C-g" 'test-command)

(define-method (test-views-procs (self <test-views>))
  (assert-equal "test-command" (command->proc-name (lookup-key test-map "C-g")))
  (assert-equal "keyboard-quit" (command->proc-name (lookup-key global-map "C-g"))))

(define-method (test-views-ref (self <test-views>))
  (let ((routes '(("^$" . #t)
                  ("^file" . #t))))
    (assert-true (match-route "" routess))
    (assert-true (match-route "file/path" routes))))
