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
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:export (list->keymap
            info
            log-info?
            fluid~
            ~
            ~/
            //
            ensure-fluid-directory
            ensure-directory))

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

(define log-info? (make-fluid #t))

;; FIXME: maybe use the emacsy logger here instead?
(define (info fmt . args)
  "Formats ARGS with FMT to current output port"
  (when (fluid-ref log-info?)
    (let ((ifmt (format #f "INFO: ~a~%" fmt)))
      (apply format #t ifmt args))))

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
