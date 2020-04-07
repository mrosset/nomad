;; util.scm
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

(define-module (nomad util)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 threads)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:export (list->keymap
            fluid~
            ~
            ~/
            //
            co-message
            ensure-directory))

(codefine* (co-message fmt . args)
           (apply message fmt args))

;;; Taken from Emacsy
(define-public (re-export-modules . modules)
  "Re-export modules"
  (define (re-export-module module)
    (module-for-each
     (lambda (sym var)
       ;; (format #t "re-exporting ~a~%" sym)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))

(define (list->keymap lst)
  "Creates a new keymap from LST"
  (let ((keymap (make-keymap)))
    (for-each (match-lambda ((key command)
                             (define-key keymap key command)))
              lst)
    keymap))

;; @var{fluid~} Expands to current users home directory
(define fluid~ (make-fluid (getenv "HOME")))

(define-syntax ~ (identifier-syntax (fluid-ref fluid~)))

(define // file-name-separator-string)

(define (~/ path)
  "Expands to the full PATH within the current users home directory"
  (string-append ~
                 //
                 path))

(define (ensure-directory dir)
  (unless (file-exists? dir)
          (mkdir dir #o755)))

(define-public (safe-message . args)
  (catch #t
    (lambda _
      (apply message args))
    (lambda (key . vals)
      (format #t "Error: key: ~a value: ~a" key vals))))

(define-public (pretty-message fmt var)
  (message fmt
           (with-output-to-string (lambda _
                                    (pretty-print var)))))

(define-public (uniquify-name str lst)
  (define (name x)
    (if (equal? x 0)
        (format #f str "")
        (format #f str x)))

  (let loop ((n 0))
    (if (not (member (name n) lst))
        (name n)
        (loop (1+ n)))))

(define-interactive (take-a-selfie)
  (message "say cheese!\n")
  (call-with-new-thread
   (lambda _
     (map (lambda (i)
            (safe-message "~a..." i)
            (sleep 1))
          '(3 2 1))
     (safe-message "~a" "click!")
     (safe-message "~a" #t)
     (usleep 75000)
     (system* "scrot" "-u")))
  #t)
