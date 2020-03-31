;; graph.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad graph)
  #:use-module (emacsy emacsy)
  #:use-module (term ansi-color)
  #:use-module (nomad platform))

(define (yellow string)
  (colorize-string string 'YELLOW))

(catch #t
  (lambda _
    (load-extension "libgv_guile.so" "SWIG_init"))
  (lambda (key . vals)
    (format #t "~a can not load guile graphviz extension libgv_guile.so\n" (yellow "WARNING:"))
    (format #t "~a key: ~a value: ~a\n" (yellow "WARNING:") key vals)))

(define (format-slots class)
  (let ((slots (class-direct-slots class)))
    (if slots
        (string-join (map (lambda (slot)
                     (format #f "+ ~a\\l" (slot-definition-name slot)))
                   slots))
        "")))

(define (format-methods class)
  (string-join (map (lambda (method)
                      (format #f "+~a ()\\l" (generic-function-name
                                              (method-generic-function method))))
                    (class-direct-methods class))))

(define (graph-node tree child class)
  (let ((parents (class-direct-supers class)))
    (for-each (lambda (parent)
                (let* ((parent-name (symbol->string
                                     (class-name parent)))
                       (node (node tree parent-name)))
                  (setv node "shape" "record")
                  ;; FIXME: due to GTK this creates to large of a map. only show slots for nomad classes
                  ;; (setv child "label" (string-append "{\\N|" (format-slots class) "}"))
                  (edge node child)
                  (graph-node tree node parent)))
              parents)))

(define-interactive (graph-class #:optional
                                     (fmt "png")
                                     (name (completing-read "Class: " (map symbol->string platform-classes))))
  (let* ((tree    (graph "buffer"))
         (class   (eval (string->symbol name)
                                   (interaction-environment)))
         (child   (node tree name))
         (file    (string-append "/tmp/dot." fmt)))
    (graph-node tree child class)
    (setv child "shape" "record")
    (layout tree "dot")
    (render tree fmt file)
    (make <webview-buffer> #:init-uri (string-append "file://" file))
    #t))

(define-key global-map (kbd "C-h 3") 'graph-class)
