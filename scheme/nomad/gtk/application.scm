;; gtk.scm
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

(define-module (nomad gtk application)
  #:use-module (nomad application)
  #:use-module (nomad init)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<nomad-gtk-application>))

(eval-when (expand load eval)
  (gi-import-by-name "Gtk" "Widget"))

(define-class <nomad-gtk-application> (<application> <gtk-application>))

(define-method (activate-cb app)
  (let ((frame (gtk-frame-new)))
    (gtk-widget-show-all frame)))

(define-method (initialize (self <nomad-gtk-application>) args)
  (next-method)
  (connect self 'activate activate-cb))
