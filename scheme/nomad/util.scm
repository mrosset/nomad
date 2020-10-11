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
  #:export (define-each-key
            fluid~
            ~
            ~/
            //
            pretty-string
            co-message
            nomad-get-version
            undefined-command
            ensure-directory))

(define (nomad-get-version) ((@ (g-golf) nomad-get-version)))

(define (pretty-string sym)
  (with-output-to-string (lambda _
                           (pretty-print sym))))

(codefine* (co-message fmt . args)
          (apply message fmt args))

(define (undefined-command command)
  ((colambda _ (message "undefined command '~a"
                        command))))

(define-public (make-buffer class . args)
  (let ((buffer (apply make class args)))
    (add-buffer! buffer)
    (switch-to-buffer buffer)))

;;; Taken from Emacsy
(define-public (re-export-modules . modules)
  "Re-export modules"
  (define (re-export-module module)
    (module-for-each
     (lambda (sym var)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))

(define (define-each-key key-map lst)
  "Defines each key command pair in @var{lst} using @var{key-map}."
  (for-each (match-lambda ((key . command)
                           (define-key key-map (kbd key) command)))
            lst))

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

(define-interactive (nomad-version)
  "Returns a string describing the version of Nomad running.  When
@var{emacsy-interactive?} is true then it also messages to the echo area."
  (if emacsy-interactive?
      (message "~a" (nomad-get-version))
      (nomad-get-version)))

(define-interactive (take-a-selfie)
  (message "📷 Click!")
  (system* "scrot" "-u")
  #t)
