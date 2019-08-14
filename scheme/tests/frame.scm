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

(define-module (tests frame)
  #:use-module (g-golf)
  #:use-module (nomad frame)
  #:use-module (srfi srfi-64)
  )

(gi-import "Gtk")
(gi-import "Nomad")

(test-begin "frame")

(let ((gtk? (gtk-init-check #f #f)))
    (test-assert "Gtk init?" gtk?))


(let* ((app (make <nomad-app>))
       (frame (make <nomad-app-frame>))
       (box (make <gtk-box>))
       (notebook (make <gtk-notebook>))
       (page (gtk-notebook-insert-page notebook box (make <gtk-label>)
                                          0))
       )

  (test-assert (not (unspecified? frame)))
  (test-equal <nomad-app-frame> (class-of frame))
  (test-equal <gtk-box> (class-of box))
  (test-equal 0 page)
  ;; (test-equal <gtk-window> (class-of (gtk-window-new 'toplevel)))

  ;; (test-assert (notebook-contains frame box))
  )

  ;; (test-error (notebook-contains frame box)))
(test-end)
