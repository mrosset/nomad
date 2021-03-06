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

;; FIXME: merge util module into app?
(define-module (nomad util)
  #:use-module (nomad lib)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (info
            log-info?
            list->keymap
            add-to-nomad-path
            ~ // ~/))

(load-extension (dynamic-path) "init_guile_nomad_util")

(define (list->keymap lst)
  "Creates a new keymap from LST"
  (let ((keymap (make-keymap)))
    (for-each (match-lambda ((key command)
                             (define-key keymap key command)))
              lst)
    keymap))

(define-public (debug-object object)
  (format #t
          "type: ~a value: ~a~%"
          (class-of object)
          object))

(define log-info? (make-fluid #t))

;; FIXME: maybe use the emacsy logger here instead?
(define (info fmt . args)
  "Formats ARGS with FMT to current output port"
  (when (fluid-ref log-info?)
    (let ((ifmt (format #f "INFO: ~a~%" fmt)))
      (apply format #t ifmt args))))

(define (user-home)
  "Returns the current users home directory"
  (let* ((user (getlogin))
         (pw (getpw user)))
    (passwd:dir pw)))

;; Expands to current users home directory
;;
;; FIXME: What's happening here? does this expand at compile time or run
;; time?
(define ~ (make-fluid (getenv "HOME")))

(define // file-name-separator-string)

(define (~/ path)
  "Expands to the full PATH within the current users home directory"
  (string-append (fluid-ref ~)
                 //
                 path))
