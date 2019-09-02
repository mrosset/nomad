;; frame.scm
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

(define-module (nomad frame)
  #:use-module (emacsy emacsy)
  #:use-module (nomad lib)
  #:use-module (nomad util)
  #:use-module (g-golf)
  #:export (make-frame-socket))

(load-extension (dynamic-path) "init_guile_nomad_frame")

(gi-import "Nomad")
(import-objects "Gtk" '("Widget" "Notebook" "Label"))

(define-public (current-notebook)
  "Returns the current notebook"
  (let* ((frame (nomad-app-get-frame))
        (notebook (nomad-app-frame-get-notebook frame)))
    notebook))

;; (define-public (switch-to-widget buffer)
;;   "Switches to buffer's widget"
;;   #t)

(define-interactive (toggle-tabs)
  "Toggles the current notebook tabs on or off."
  (let ((notebook (current-notebook)))
    (gtk-notebook-set-show-tabs notebook
                                (not (gtk-notebook-get-show-tabs notebook)))
    (gtk-notebook-get-show-tabs notebook)))

(define-public (notebook-insert buffer position)
  "Inserts a BUFFER with POSITION into the current frame's notebook"
  (let* ((widget (slot-ref buffer 'widget))
         (frame (nomad-app-get-frame))
         (notebook (nomad-app-frame-get-notebook frame))
         (label (gtk-label-new (buffer-name buffer))))
    (gtk-notebook-insert-page notebook
                              widget
                              label
                              position)
    (gtk-widget-show-all widget)))

(define-public (grab-readline)
  (let* ((frame (nomad-app-get-frame))
         (readline (nomad-app-frame-get-readline frame)))
    (gtk-widget-grab-focus readline)))

(define-public (number-tabs)
  (let* ((frame (nomad-app-get-frame))
         (notebook (nomad-app-frame-get-notebook frame)))
    (gtk-notebook-get-n-pages notebook)))

(define (make-frame-socket url socket)
  "Write `make-frame' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-frame ,url)) socket))

;; (define-public (notebook-contains notebook buffer)
;;   "Returns true if the current frames notebook contains BUFFER"
;;   (let* ((page (gtk-notebook-page-num notebook
;;                                       (slot-ref buffer 'widget))))
;;     (if (>= page 0)
;;         #t
;;         #f)))
