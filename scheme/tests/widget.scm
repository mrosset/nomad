;; widget.scm
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

(define-module (tests webview)
  #:use-module (oop goops)
  #:use-module (nomad widget)
  #:use-module ((nomad webview) #:select (<webview-buffer>))
  #:use-module (g-golf)
  #:use-module (nomad util)
  #:use-module (srfi srfi-64)
  )

(import-functions "Gtk" '("init_check"))

(import-objects "Gtk" '("Widget" "Label"))

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "Gtk init?" gtk?))

(test-group "widget"
            (let ((buffer (make <widget-buffer>))
                  (view (make <webview-buffer>)))
              (slot-set! buffer 'widget (make <gtk-label>))
              (test-assert (not (null-pointer? (buffer-pointer buffer))))
              (test-equal <gtk-label> (class-of (buffer-widget buffer)))
              (test-equal <webview-buffer> (class-of (buffer-widget view)))
              ))
