;; gi.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad gtk gi)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

(set! %gi-method-short-names-skip '(insert
                                    reload
                                    load-uri))

(eval-when (expand load eval)
  (gi-import "Gdk")
  (gi-import "GdkX11")
  (map (match-lambda ((namespace item)
                      (gi-import-by-name namespace item)))
       '(("Gtk" "Application")
         ("Gtk" "ApplicationWindow")
         ("Gtk" "Clipboard")
         ("Gtk" "DrawingArea")
         ("Gtk" "VBox")
         ("Gtk" "HBox")
         ("Gtk" "ScrolledWindow")
         ("Gtk" "Viewport")
         ("Gtk" "Label")
         ("Gtk" "Button")
         ("GtkSource" "View")
         ("GtkSource" "Buffer")
         ("GtkSource" "Language")
         ("GtkSource" "LanguageManager")
         ("GtkSource" "StyleScheme")
         ("GtkSource" "StyleSchemeManager")))
  (gi-import "Vte")
  (gi-import "WebKit2")
  (gi-import "Nomad"))
