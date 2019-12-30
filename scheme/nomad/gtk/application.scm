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
  #:use-module (oop goops)
  #:use-module (nomad webview)
  #:use-module (g-golf)
  #:export (<nomad-gtk-application>))

(eval-when (expand load eval)
  (for-each (lambda (x)
              (gi-import-by-name (car x) (cdr x)))
            '(("Gtk" . "Application")
              ("Gtk" . "Container")
              ("Gtk" . "ApplicationWindow")
              ("Gtk" . "Widget")
              ("Gtk" . "VBox")
              ("Gtk" . "Label")
              ("Gtk" . "Button")
              ("Gtk" . "init")
              ("WebKit2" . "WebView"))))

(define-class <nomad-gtk-application> (<application> <gtk-application>))

(define-method (activate-closure app)
  (let ((window (gtk-application-window-new app))
        (view (make <webkit-web-view>))
        (box (make <gtk-vbox> #:spacing 0)))
    (webkit-web-view-load-uri view default-home-page)
    (gtk-container-add window box)
    (gtk-box-pack-start box view #t #t 0)
    (gtk-widget-show-all window)))

(define-method (initialize (self <nomad-gtk-application>) args)
  (next-method)
  (connect self 'activate activate-closure))
