;; buffers.scm
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

(define-module (nomad gtk buffers)
  #:use-module (ice-9 format)
  #:use-module (emacsy emacsy)
  #:use-module (nomad api)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk window)
  #:use-module (nomad gtk frame)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<gtk-widget-buffer>
            <gtk-textview-buffer>
            <gtk-webview-buffer>
            <gtk-popup-buffer>
            widget-uri
            widget-title
            buffer-load-uri
            buffer-back
            buffer-forward
            buffer-hints
            hints-finish
            buffer-scroll-up
            buffer-scroll-down
            buffer-reload
            search-forward
            search-finish))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("Gtk" . "Widget")
         ("Gtk" . "DrawingArea")
         ("Gtk" . "ApplicationWindow")
         ("Gtk" . "Notebook")
         ("Gtk" . "Grid")
         ("Gtk" . "TextBuffer")
         ("Gtk" . "ScrolledWindow")
         ("GtkSource" . "View")))
  (gi-import "WebKit2")
  (gi-import "Nomad"))


;; Methods
;;
;; <web-buffer>

(define-method (emacsy-mode-line (buffer <web-buffer>))
  (format #f "~a~/~a~/~a~/~a%"
          (next-method)
          (buffer-title buffer)
          (buffer-uri buffer)
          (buffer-progress buffer)))

(define-method (initialize (buffer <web-buffer>) arg)
  (next-method)
  (set! (buffer-modes buffer) `(,web-mode))
  (set! (buffer-widget buffer) (make <widget-web-view> #:buffer buffer)))

(define-method (widget-title (buffer <web-buffer>))
  (if (!is-loading (buffer-widget buffer))
      "loading..."
      (webkit-web-view-get-title (buffer-widget buffer))))

(define-method (widget-uri (buffer <web-buffer>))
  (webkit-web-view-get-uri (buffer-widget buffer)))

(define-method (buffer-load-uri (buffer <web-buffer>) uri)
  (webkit-web-view-load-uri (buffer-widget buffer)
                            uri))

(define-method (buffer-forward (buffer <web-buffer>))
  (webkit-web-view-go-forward (buffer-widget buffer)))

(define-method (buffer-back (buffer <web-buffer>))
  (webkit-web-view-go-back (buffer-widget buffer)))

(define-method (buffer-reload (buffer <web-buffer>))
  (webkit-web-view-reload (buffer-widget buffer)))

(define-method (buffer-scroll-up (buffer <web-buffer>))
  (nomad-app-run-javascript (buffer-widget buffer)
                            "window.scrollBy(0, -25);"))

(define-method (buffer-scroll-down (buffer <web-buffer>))
  (nomad-app-run-javascript (buffer-widget buffer)
                            "window.scrollBy(0, 25);"))

(define-method (hints-finish (buffer <web-buffer>))
  (nomad-app-send-message (buffer-widget buffer)
                          (make <webkit-user-message> #:name "hints-finish")))

(define-method (buffer-hints (buffer <web-buffer>))
  (nomad-app-send-message (buffer-widget buffer)
                          (make <webkit-user-message> #:name "show-hints")))

(define-method (search-forward (buffer <web-buffer>))
  (let ((controller (webkit-web-view-get-find-controller (buffer-widget buffer))))
    (webkit-find-controller-search controller (current-search buffer) 1 255)))

(define-method (search-finish (buffer <web-buffer>))
  (set! (current-search buffer) #f)
  (let ((controller (webkit-web-view-get-find-controller (buffer-widget buffer))))
    (webkit-find-controller-search-finish controller)))




(define-class <gtk-widget-buffer> ())

(define-method (initialize (self <gtk-widget-buffer>) args)
  (next-method)
  (add-hook! (buffer-kill-hook self)
             (lambda _
               (gtk-widget-destroy self)))
  (switch-to-buffer self))


