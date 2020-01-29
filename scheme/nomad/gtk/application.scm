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
  #:use-module (nomad platform api)
  #:use-module (nomad init)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<nomad-gtk-application>))

(eval-when (expand load eval)
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("Gtk" . "Widget")
         ("Gtk" . "Application"))))

(define-class <nomad-gtk-application> (<nomad-application> <gtk-application>))

(define-method (activate-cb app)
  (gtk-widget-show-all (gtk-frame-new app))
  (make <gtk-webview-buffer>)
  (app-init))

(define-method (initialize (self <nomad-gtk-application>) args)
  (next-method)
  (connect self 'activate activate-cb))
