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
  #:use-module (g-golf)
  #:use-module (nomad app)
  #:use-module (nomad text)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign))

(gi-import "Gtk")
(gi-import "GtkSource")

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "GTK init" gtk?)
  (unless gtk?
    (test-skip "source is pointer?")))

(test-assert "source is source view?" (equal? (class-of (make <gtk-source-view>)) <gtk-source-view>))
