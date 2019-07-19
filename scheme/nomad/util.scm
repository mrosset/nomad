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
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (info
            list->keymap
            add-to-nomad-path
            ~ // ~/))

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

(define (info msg)
  (format #t "INFO: ~a\n" msg))

(define (user-home)
  "Returns the current users home directory"
  (let* ((user (getlogin))
         (pw (getpw user)))
    (passwd:dir pw)))

;; Expands to current users home directory
(define ~
  (make-fluid (user-home)))

(define // file-name-separator-string)

(define (~/ path)
  "Expands to the full PATH within the current users home directory"
  (string-append (fluid-ref ~)
                 //
                 path))
