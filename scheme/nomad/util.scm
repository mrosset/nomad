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
  #:use-module (oop goops)
  #:export (info
            add-to-nomad-path
            ~ // ~/))


(define-public (debug-object object)
  (format #t
          "type: ~a value: ~a~%"
          (class-of object)
          object))

(define (info msg)
  (format #t "INFO: ~a\n" msg))

(define ~
  (make-fluid (getenv "HOME")))

(define // file-name-separator-string)

(define (~/ path)
  "expands $HOME and joins path to the end"
  (string-append (fluid-ref ~) // path))

(define (add-to-nomad-path path)
  (add-to-load-path path))

(define-public (completion-join lst)
  "Joins completion list into a flat string separated by spaces"
  (string-append lst))
