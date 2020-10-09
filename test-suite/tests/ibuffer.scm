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

(define buffer-name-at-line (@@ (nomad ibuffer) buffer-name-at-line))
(define entry-at-line (@@ (nomad ibuffer) entry-at-line))
(define apply-marks (@@ (nomad ibuffer) apply-marks))

(define mark-entry (@@ (nomad ibuffer) mark-entry))
(define unmark (@@ (nomad ibuffer) unmark))

(define marked? (@@ (nomad ibuffer) marked?))

(define-method (test-ibuffer-entry (self <test-ibuffer>))
  (emacsy-initialize #t)
  (let* ((entry (make <ibuffer-entry>
                  #:buffer (make <text-buffer> #:name "test-buffer")))  )
    (assert-true (is-a? (slot-ref entry 'buffer) <buffer>))
    (add-buffer! (slot-ref entry 'buffer))
    (assert-equal 3 (length (buffer-list)))
    (assert-false (marked? entry))
    ;; mark
    (mark-entry entry 'D)
    (assert-true (marked? entry))
    ;; umark
    (unmark entry)
    (assert-false (marked? entry))
    ;; apply-marks
    (mark-entry entry 'D)
    (assert-true (marked? entry))
    (apply-marks entry)
    (assert-false (marked? entry))
    (assert-equal 2 (length (buffer-list)))))

(define-method (test-interactive (self <test-ibuffer>))
  (emacsy-initialize #t)
  (set! current-window (make <window> #:window-buffer (current-buffer)))
  (make-test-buffers)
  (assert-equal 6 (length (buffer-list)))
  (ibuffer)
  (assert-equal "ibuffer" (buffer-name))
  (assert-equal 3 (line-number-at-pos))
  (assert-equal "*Messages*" (buffer-name-at-line))
  (ibuffer-forward-line 2)
  (assert-equal 5 (line-number-at-pos))
  (ibuffer-forward-line)
  (ibuffer-mark-delete)
  (ibuffer-backward-line)
  (assert-true (marked? (entry-at-line)))
  (emacsy-key-event #\x)
  (emacsy-key-event #\y)
  (emacsy-key-event #\return)
  (agenda-schedule (colambda _
                    (ibuffer-do-kill-on-deletion-marks)))
  (update-agenda)
  (assert-equal 6 (length (buffer-list)))
  (assert-equal "*scratch*" (buffer-name-at-line)))
