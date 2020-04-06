;; log.scm
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

(define-module (nomad log)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (term ansi-color)
  #:use-module (logging logger)
  #:use-module (logging rotating-log)
  #:use-module (logging port-log)
  #:export (log-formatter
            log-info
            log-warn
            log-crit
            log-debug))

(define (log-info . msg)
  (log-msg 'INFO msg))

(define (log-warn . msg)
  (log-msg 'WARN msg))

(define (log-crit . msg)
  (log-msg 'CRITICAL msg))

(define (log-debug . msg)
  (log-msg 'DEBUG msg))

(define level-alist '((CRITICAL . RED)
                      (DEBUG . MAGENTA)
                      (WARN . YELLOW)
                      (INFO . BLUE)))

(eval-when (expand load eval)
  ;; Same as default-log-formatter but colorized
  (define (log-formatter lvl time str)
    (let ((color (or (assoc-ref level-alist lvl) 'CLEAR)))
      (format #f "~a (~a): ~a~%"
              (strftime "%F %H:%M:%S" (localtime time))
              (colorize-string (symbol->string lvl) color)
              str)))
  (let ((lgr     (make <logger>))
        (handler (make <port-log>
                   #:port (current-error-port)
                   #:formatter log-formatter)))
    (register-logger! "nomad" lgr)
    (add-handler! lgr handler)
    (set-default-logger! lgr)))
