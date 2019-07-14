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
  (display "  " port)
  (display (car pair) port)
  (display ": " port)
  (write (cdr pair) port)
  (newline port))

(define (test-on-test-end-simple runner log)
  (let ((kind (test-result-ref runner 'result-kind)))
    (if (memq kind
              '(fail xpass))
        (let* ((results (test-result-alist runner))
               (source-file (assq 'source-file results))
               (source-line (assq 'source-line results))
               (test-name (assq 'test-name results)))
          (if (or source-file source-line)
              (begin (if source-file
                         (display (cdr source-file)))
                     (display ":")
                     (if source-line
                         (display (cdr source-line)))
                     (display ": ")))
          (display (if (eq? kind 'xpass)
                       "XPASS"
                       "FAIL"))
          (if test-name
              (begin (display " ")
                     (display (cdr test-name))))
          (newline)))
    (if (output-port? log)
        (begin (display "Test end:" log)
               (newline log)
               (let loop
                   ((list (test-result-alist runner)))
                 (if (pair? list)
                     (let ((pair (car list)))
                       ;; Write out properties not written out by on-test-begin.
                       (if (not (memq (car pair)
                                      '(test-name source-file source-line source-form)))
                           (%test-write-result1 pair log))
                       (loop (cdr list)))))
               ))))

(define (test-on-test-begin-simple runner log)
  (if (output-port? log)
      (let* ((results (test-result-alist runner))
             (source-file (assq 'source-file results))
             (source-line (assq 'source-line results))
             (source-form (assq 'source-form results))
             (test-name (assq 'test-name results)))
        (display "Test begin:" log)
        (newline log)
        (if test-name
            (%test-write-result1 test-name log))
        (if source-file
            (%test-write-result1 source-file log))
        (if source-line
            (%test-write-result1 source-line log))
        (if source-form
            (%test-write-result1 source-form log)))))

(define (html-simple-runner filename)
  (let ((runner (test-runner-null))
        (port (open-output-file filename))
        (num-passed 0)
        (num-failed 0))
    (display "<pre><code>" port)
    (test-runner-on-test-begin! runner
      (lambda (runner)
        (test-on-test-begin-simple runner port)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass)
           (set! num-passed
                 (+ num-passed 1)))
          ((fail xfail)
           (set! num-failed
                 (+ num-failed 1)))
          (else #t))
        (test-on-test-end-simple runner port)))
    (test-runner-on-final! runner
      (lambda (runner)
        (format port
                "</code></pre><p><font color=\"~a\"> Passing tests: ~d. Failing tests: ~d.</font>~%</div>"
                (if (> num-failed 0) "red" "green")
                num-passed
                num-failed)
        (close-output-port port)))
    runner))

(define-interactive (run-graphical-tests)
  (test-runner-factory (lambda ()
                         (html-simple-runner "graphical.log")))
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
