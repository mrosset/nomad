;; ibuffer.scm
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

(define-module (tests ibuffer)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad emacsy text)
  #:use-module (nomad ibuffer)
  #:use-module (unit-test))

(define-class <test-ibuffer> (<test-case>))

(define-interactive (make-test-buffers)
  (do ((i 0 (1+ i)))
      ((> i 3))
    (let ((name (format #f "test-buffer-~a" i)))
      (add-buffer! (make <text-buffer> #:name name)))))

(define buffer-at-line (@@ (nomad ibuffer) buffer-at-line))

(define-method (test-interactive (self <test-ibuffer>))
  (emacsy-initialize #t)
  (set! current-window (make <window> #:window-buffer (current-buffer)))
  (make-test-buffers)
  (assert-equal 6 (length (buffer-list)))
  (ibuffer)
  (assert-equal "ibuffer" (buffer-name))
  (assert-equal 3 (line-number-at-pos))
  (ibuffer-forward-line 2)
  (assert-equal 5 (line-number-at-pos))
  (ibuffer-kill-buffer)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal "test-buffer-0" (buffer-name-at-line)))
