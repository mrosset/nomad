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

(gi-import "Nomad")
(import-objects "Gtk" '("Widget" "Notebook" "Label"))

(define-public (current-notebook)
  "Returns the current notebook"
  (let* ((frame (current-frame))
        (notebook (nomad-app-frame-get-notebook frame)))
    notebook))

(define-public (current-frame)
  "Returns the current frame"
  (nomad-app-get-frame))

(define-public (current-notebook-widget notebook)
  "Returns the NOTEBOOK's widget"
  (let ((page (gtk-notebook-get-current-page notebook)))
    (gtk-notebook-get-nth-page notebook page)))

(define-public (current-echo-area)
  "Returns the echo area for the current frame"
  (nomad-app-frame-get-readline (current-frame)))

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

(define-public (switch-to-buffer-widget widget)
  "Switched to the BUFFER widget found in the current notebook. This should
only be used to sync switch-to-buffer with it's GTK widget"
  (let* ((notebook (current-notebook))
         (page (gtk-notebook-page-num notebook widget)))
    ;; If the notebook does not contain the widget, then add it
    (when (< page 0)
      (set! page
            (gtk-notebook-append-page notebook widget
                                      #f)))
    ;; (when (eq? <webview-buffer> (class-of buffer))
    ;;   (gtk-notebook-set-tab-label-text notebook widget page)
    ;;   )
    ;; Switch to the widget's page
    (gtk-widget-show-all widget)
    (gtk-notebook-set-current-page notebook page)))
;; (define-public (notebook-contains notebook buffer)
;;   "Returns true if the current frames notebook contains BUFFER"
;;   (let* ((page (gtk-notebook-page-num notebook
;;                                       (slot-ref buffer 'widget))))
;;     (if (>= page 0)
;;         #t
;;         #f)))
