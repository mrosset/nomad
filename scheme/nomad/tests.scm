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
  #:use-module (nomad frame)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))


(define (test-on-group-begin-simple runner suite-name
                                    count)
  (if (null? (test-runner-group-stack runner))
      (begin (display "%%%% Starting test ")
             (display suite-name)
             (if test-log-to-file
                 (let* ((log-file-name (if (string? test-log-to-file)
                                           test-log-to-file
                                           (string-append suite-name ".log")))
                        (log-file (cond-expand (mzscheme (open-output-file log-file-name 'truncate/replace))
                                               (else (open-output-file log-file-name)))))
                   (display "<pre><code>" log-file)
                   (display "%%%% Starting test " log-file)
                   (display suite-name log-file)
                   (newline log-file)
                   (test-runner-aux-value! runner log-file)
                   (display "  (Writing full log to \"")
                   (display log-file-name)
                   (display "\")")))
             (newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (begin (display "Group begin: " log)
               (display suite-name log)
               (newline log))))
  #f)

(define (html-simple-runner)
  (let ((runner (test-runner-simple))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
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
        (test-on-test-end-simple runner)))
    (test-runner-on-final! runner
      (lambda (runner)
        (let ((log (test-runner-aux-value runner)))
          (format log
                  "</code></pre><p><font color=~s> Passing tests: ~d. Failing tests: ~d.</font>~%</div>"
                  (if (> num-failed 0)
                      "red"
                      "green")
                  num-passed
                  num-failed)
          (close-output-port log))))
    runner))

(define-interactive (run-graphical-tests)
  (test-runner-factory html-simple-runner)
  (kill-some-buffers)
  (test-begin "graphical")
  (test-equal "buffer name"
    "gnu.org"
    (begin (make-buffer "gnu.org")
           (buffer-name (current-buffer))))
  (test-equal "browse"
    "https://gnu.org/"
    (begin (browse "gnu.org")
           (buffer-uri)))
  (test-equal "notebook-contain"
    #t
    (notebook-contains (current-buffer)))
  (test-equal "kill-buffer"
    "*scratch*"
    (begin (kill-buffer)
           (buffer-name (current-buffer))))
  (redisplay-buffers)
  (test-equal "tab and buffer synchronization"
    (length (buffer-list))
    (number-tabs))
  (do ((i 0
          (+ 1 i)))
      ((> i 10))
    (make-content-buffer (number->string i)
                         (format #f "<h2>ID: ~a</h2>" i)))
  ;; visit each buffer
  (for-each (lambda (buffer)
              (next-buffer))
            (buffer-list))
  (kill-some-buffers)
  (redisplay-buffers)
  ;; total tabs equals total buffers
  (test-equal "tab and buffer synchronization"
    (length (buffer-list))
    (number-tabs))
  (test-end)
  ;; Display test results in a content buffer
  (let* ((log-file "graphical.log")
         (port (open-input-file log-file))
         (content (get-string-all port)))
    (make-content-buffer log-file content)
    (close-port port)
    (delete-file log-file))
    (with-buffer scratch (kill-buffer)))
