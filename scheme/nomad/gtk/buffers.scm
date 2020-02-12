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
  #:use-module (emacsy emacsy)
  #:use-module (nomad api)
  #:use-module (nomad util)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<gtk-widget-buffer>
            <gtk-textview-buffer>
            <gtk-webview-buffer>
            <gtk-popup-buffer>
            buffer-uri
            buffer-load-uri
            buffer-back
            buffer-forward
            buffer-hints
            hints-finish
            buffer-scroll-up
            buffer-scroll-down
            buffer-reload
            current-search
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



(define-class <gtk-widget-buffer> ()
  (container #:accessor !container #:init-keyword #:container #:init-value #f))

(define-method (initialize (self <gtk-widget-buffer>) args)
  (next-method)

  (unless (!container self)
       (let* ((app (g-application-get-default))
              (frame (gtk-application-get-active-window app))
              (container (slot-ref frame 'container)))
         (set! (!container self) container)))

  (gtk-container-add (!container self) self)
  (gtk-widget-show-all (!container self))

  (add-hook! (buffer-enter-hook self)
             (lambda _
               (let* ((notebook (!container self))
                      (page (gtk-notebook-page-num notebook self)))
                 (gtk-notebook-set-current-page notebook page)
                 (gtk-notebook-set-tab-label-text notebook self (format #f "~a" page))
                 (gtk-widget-show-all notebook)
                 (gtk-widget-grab-focus self))))

  (add-hook! (buffer-kill-hook self)
             (lambda _
               (gtk-widget-destroy self)))
  (switch-to-buffer self))



(define-class <gtk-popup-buffer> (<nomad-buffer> <gtk-grid>)
  (container #:accessor !container #:init-keyword #:container #:init-value #f)
  (list #:accessor !list #:init-keyword #:list #:init-value '()))

(define-method (initialize (self <gtk-popup-buffer>) args)
  (next-method)
  (unless (!container self)
       (set! (!container self) (!mini-popup (current-frame))))

  (let* ((popup (!container self))
         (items (!list self))
         (grid  (!grid popup)))
    (do ((i 0
          (+ 1 i)))
        ((>= i (length items)))
      (gtk-grid-attach grid
                       (make <gtk-label>
                         #:label
                         (buffer-name (list-ref items i)))
                       0 i 1 1))

    (add-hook! (buffer-enter-hook self)
               (lambda _
                 (gtk-widget-show-all popup)))

    (add-hook! (buffer-exit-hook self)
               (lambda _
                 (gtk-widget-hide popup)))
    (add-hook! (buffer-kill-hook self)
               (lambda _
                 (gtk-widget-hide popup))))
    (switch-to-buffer self))

(define-class <gtk-webview-buffer> (<gtk-widget-buffer>
                                    <nomad-webview-buffer>
                                    <webkit-web-view>)
  (search #:accessor current-search #:init-value #f))

(define-method (initialize (self <gtk-webview-buffer>) args)
  (next-method)
  (connect self 'load-changed
           (lambda _
             (let ((percent (inexact->exact
                             (round (* 100 (!estimated-load-progress self))))))
               (slot-set! self 'name (!uri self)))
             #t))

  (connect self 'user-message-received
           (lambda (v m)
             (safe-message "~a" (webkit-user-message-get-name m))))

  (buffer-load-uri self (!init-uri self)))

(define-method (buffer-load-uri (self <gtk-webview-buffer>) uri)
  (webkit-web-view-load-uri self uri))

(define-method (buffer-uri (self <gtk-webview-buffer>))
   (webkit-web-view-get-uri self))

(define-method (buffer-forward (self <gtk-webview-buffer>))
  (webkit-web-view-go-forward self))

(define-method (buffer-back (self <gtk-webview-buffer>))
  (webkit-web-view-go-back self))

(define-method (buffer-reload (self <gtk-webview-buffer>))
  (webkit-web-view-reload self))

(define-method (buffer-scroll-up (self <gtk-webview-buffer>))
  (nomad-app-run-javascript self "window.scrollBy(0, -25);"))

(define-method (buffer-scroll-down (self <gtk-webview-buffer>))
  (nomad-app-run-javascript self "window.scrollBy(0, 25);"))

(define-public message-reply #f)

(define-method (hints-finish (self <gtk-webview-buffer>))
  (nomad-app-send-message self
                          (make <webkit-user-message> #:name "hints-finish")))

(define-method (buffer-hints (self <gtk-webview-buffer>))
  (nomad-app-send-message self
                          (make <webkit-user-message> #:name "show-hints")))

(define-method (search-forward (self <gtk-webview-buffer>))
  (let ((controller (webkit-web-view-get-find-controller self)))
    (webkit-find-controller-search controller (current-search self) 1 255)))

(define-method (search-finish (self <gtk-webview-buffer>))
  (set! (current-search self) #f)
  (let ((controller (webkit-web-view-get-find-controller self)))
    (webkit-find-controller-search-finish controller)))



(define-class <gtk-textview-buffer> (<nomad-text-buffer>
                                     <gtk-widget-buffer>
                                     <gtk-scrolled-window>)
  (source-view #:accessor !source-view)
  (name #:init-value "<gtk-textview-buffer>"))

(define-method (initialize (self <gtk-textview-buffer>) args)
  (next-method)
  (let ((view (make <widget-source-view>
                              #:theme "classic"
                              #:top-margin 1
                              #:bottom-margin 1
                              #:buffer self
                              #:thunk (lambda _
                                        (buffer:buffer-string self)))))
    (set! (!source-view self) view)
    (gtk-container-add self view)
    (gtk-widget-show-all self)
    (gtk-widget-grab-focus view)))


