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
  #:use-module (nomad util)
  #:use-module (srfi srfi-64)
  )

(eval-when (expand load eval)
  (gi-import "Nomad")
  (gi-import "GtkSource")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "init_check")
              ("Gtk" . "Notebook"))))

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert gtk?))

(test-group "frame"
            (let* ((frame (make <nomad-app-frame>))
                   (readline (nomad-app-frame-get-readline frame))
                   (notebook (nomad-app-frame-get-notebook frame))
                   (view (make <gtk-source-view>))
                   (page (gtk-notebook-append-page notebook view #f)))

              (test-equal <gtk-source-view> (class-of readline))
              (test-equal <gtk-notebook> (class-of notebook))
              (test-equal <nomad-app-frame> (class-of frame))
              (test-equal 0 page)
              (test-equal <gtk-source-view> (class-of (current-notebook-widget notebook)) )
              ))
