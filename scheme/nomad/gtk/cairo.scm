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

(define-module (nomad gtk cairo)
  #:use-module (cairo)
  #:use-module (emacsy emacsy)
  #:use-module (nomad gtk frame)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module (g-golf)
  #:export (<gtk-cairo-buffer>))

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
