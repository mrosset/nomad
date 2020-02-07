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

(define-module (test text)
  #:use-module (emacsy emacsy)
  #:use-module (g-golf)
  #:use-module (nomad text)
  #:use-module (nomad util)
  #:use-module (srfi srfi-64))

(gi-import "GtkSource")

(for-each (lambda (x)
            (gi-import-by-name "Gtk" x))
          '("init_check" "ScrolledWindow" "TextView"))

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "GTK init" gtk?))

(test-group "text"
            (let ((buffer (switch-to-buffer "*text-buffer*")))
              (test-equal <text-buffer> (class-of buffer))
              (text-buffer->nomad-text-buffer! buffer)
              (test-equal <nomad-text-buffer> (class-of buffer))
              (test-equal <gtk-scrolled-window> (class-of (buffer-widget buffer)))
              (test-equal <gtk-source-view> (class-of (get-source-widget (buffer-widget buffer))))))
