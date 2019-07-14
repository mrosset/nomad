;; tests.scm
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

(define-module (nomad tests)
  #:use-module (emacsy emacsy)
  #:use-module (nomad buffer)
  #:use-module (nomad webview)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(define (%test-write-result1 pair port)
  (format port "<p>~a : ~a</p>" (car pair) (cdr pair)))

(define (html-simple-runner filename)
  (let ((runner (test-runner-null))
        (port (open-output-file filename))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass)
           (set! num-passed
                 (+ num-passed 1)))
          ((fail xfail)
           (set! num-failed
                 (+ num-failed 1)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format port "<p>Passing tests: ~d.~%Failing tests: ~d.~%</p>"
                num-passed num-failed)
        (close-output-port port)))
    runner))

(define-interactive (run-graphical-tests)
  ;; (test-runner-factory (lambda ()
  ;;                      (html-simple-runner "tests.log")))

  (test-begin "graphical")
  (begin (kill-some-buffers)
         (test-equal "https://bufio.org/"
           (begin (make-buffer "bufio.org")
                  (update-buffer-names)
                  (switch-to-buffer "https://bufio.org/")
                  (buffer-name (current-buffer))))
         (test-equal "*scratch*"
           (begin (kill-buffer)
                  (buffer-name (current-buffer)))))
  (test-end)
  (let* ((port (open-input-file "graphical.log"))
         (log (get-string-all port)))
    (make-buffer "https://www.google.ca")
    (load-content log)
    (close-port port)))
