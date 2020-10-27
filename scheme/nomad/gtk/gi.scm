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

(set! %gi-method-short-names-skip '(query
                                    setenv
                                    insert
                                    reload
                                    load-uri))

(eval-when (expand load eval)
  (gi-import "Gdk")
  (gi-import "GdkX11")
  (map (match-lambda ((namespace item)
                      (gi-import-by-name namespace item)))
       '(("Gtk" "init")
         ("Gtk" "main")
         ("Gtk" "main_quit")
         ("Gtk" "Application")
         ("Gtk" "ApplicationWindow")
         ("Gtk" "Clipboard")
         ("Gtk" "DrawingArea")
         ("Gtk" "Style")
         ("Gtk" "Button")
         ("Gtk" "Entry")
         ("Gtk" "EntryBuffer")
         ("Gtk" "IconSize")
         ("Gtk" "Image")
         ("Gtk" "VBox")
         ("Gtk" "HBox")
         ("Gtk" "Box")
         ("Gtk" "Stack")
         ("Gtk" "VSeparator")
         ("Gtk" "ScrolledWindow")
         ("Gtk" "StyleContext")
         ("Gtk" "Viewport")
         ("Gtk" "Label")
         ("Gtk" "MenuButton")
         ("Gtk" "PopoverMenu")
         ("Gtk" "MessageDialog")
         ("Gtk" "ButtonsType")
         ("Gtk" "ResponseType")
         ("Gtk" "HeaderBar")
         ("GtkSource" "View")
         ("GtkSource" "Buffer")
         ("GtkSource" "Language")
         ("GtkSource" "LanguageManager")
         ("GtkSource" "SpaceDrawer")
         ("GtkSource" "Completion")
         ("GtkSource" "StyleScheme")
         ("GtkSource" "StyleSchemeManager")))
  (gi-import "Vte")
  (gi-import "WebKit2")
  (gi-import "Nomad"))
