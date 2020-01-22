;; text.scm
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

(define-module (nomad text)
  #:use-module (emacsy emacsy)
  #:use-module (g-golf)
  #:use-module (ice-9 optargs)
  #:use-module (nomad frame)
  #:use-module (nomad util)
  #:use-module (nomad lib)
  #:use-module (nomad widget)
  #:use-module (oop goops)
  #:use-module (system foreign))

(gi-import "GtkSource")
(gi-import "GLib")

(for-each (lambda (x)
            (gi-import-by-name "Gtk" x))
          '("TextBuffer" "TextView" "Container" "ScrolledWindow"))

(define default-source-language "scheme")
(define default-source-theme "classic")

(define-class <nomad-text-buffer> (<widget-buffer>))

(define* (make-source-view #:optional theme language)
  (let ((window (make <gtk-scrolled-window>))
        (view (make <gtk-source-view>))
        (t (or theme default-source-theme))
        (l (or language default-source-language)))
    (nomad-app-source-view-set-buffer view t l)
    (gtk-container-add window view)
    window))

(define-public (get-source-widget widget)
  "Returns the source view for a WIDGET. The widget is either a container or a
source view"
  (if (eq? <gtk-scrolled-window> (class-of widget))
      (car (gtk-container-get-children widget))
      widget))

(define-public (set-source-text! view text)
  "Sets source VIEW text buffer to TEXT"
  (let ((buf (gtk-text-view-get-buffer view)))
    (gtk-text-buffer-set-text buf text -1)))

(define-public (set-source-point! view pos)
  "Sets source VIEW cursor point to POS"
  (let* ((buf (gtk-text-view-get-buffer view))
         (iter (gtk-text-buffer-get-start-iter buf)))
    (gtk-text-buffer-get-start-iter buf)
    (gtk-text-iter-forward-chars iter
                                (- pos 1))
    (gtk-text-buffer-place-cursor buf iter)
    ))

(define-public (text-buffer->nomad-text-buffer! buffer)
  "Converts a <text-buffer> class to a pointer-buffer."
  (change-class buffer <nomad-text-buffer>)
  (info "converting ~a" buffer)
  (slot-set! buffer
             'widget
             (make-source-view))
  (add-hook! (buffer-enter-hook buffer)
             widget-enter-hook)
  (add-hook! (buffer-kill-hook buffer)
             widget-kill-hook))
