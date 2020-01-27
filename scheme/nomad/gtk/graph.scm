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

(define-module (nomad gtk graph)
  #:use-module (cairo)
  #:use-module (emacsy emacsy)
  #:use-module (nomad platform api)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:use-module (g-golf)
  #:export (<gtk-cairo-buffer>))

(eval-when (expand load eval)
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("WebKit2" . "WebView"))))

(load-extension "libgv_guile.so" "SWIG_init")

(define-class <gtk-cairo-buffer> (<nomad-text-buffer>
                                  <gtk-widget-buffer>
                                  <gtk-drawing-area>)
  (name #:init-value "*cairo*"))

(define-method (initialize (self <gtk-cairo-buffer>) args)
  (next-method)
  (slot-set! self 'name "*cairo*")
  (connect self 'draw draw-class)
  (gtk-widget-show-all self))



(define (draw-class widget ctx)
  (let* ((cairo  (cairo-pointer->scm ctx))
         (width  (gtk-widget-get-allocated-width widget))
         (height (gtk-widget-get-allocated-height widget))
         (png    (cairo-image-surface-create-from-png "/tmp/dot.png"))
         (iw     (cairo-image-surface-get-width png))
         (ih     (cairo-image-surface-get-height png))
         (xscale (/ width iw))
         (hscale (/ height ih)))

    (cairo-set-source-surface cairo png 0 0)


    (cairo-paint cairo)
    (cairo-surface-destroy png)
    #f))

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
                  ;; (setv child "label" (string-append "{\\N|" (format-slots class) "}"))
                  (edge node child)
                  (graph-node tree node parent)))
              parents)))

(define-interactive (graph-class #:optional
                                     (fmt "png")
                                     (class <gtk-webview-buffer>))
  (let* ((tree    (graph "buffer"))
         (name    (symbol->string (class-name class)))
         (child   (node tree name))
         (file    (string-append "/tmp/dot." fmt)))

    (graph-node tree child class)
    (setv child "shape" "record")
    (layout tree "dot")
    (render tree fmt file)
    ;; (make <gtk-cairo-buffer>)
    (make <gtk-webview-buffer> #:init-uri (string-append "file://" file))
    #t))
