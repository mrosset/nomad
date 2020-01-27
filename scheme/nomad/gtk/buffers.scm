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
  #:use-module (nomad platform api)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<gtk-widget-buffer>
            <gtk-webview-buffer>
            <gtk-textview-buffer>
            buffer-uri
            buffer-load-uri))

(eval-when (expand load eval)
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("WebKit2" . "WebView")
         ("WebKit2" . "LoadEvent")
         ("Gtk" . "Widget")
         ("Gtk" . "DrawingArea")
         ("Gtk" . "ApplicationWindow")
         ("Gtk" . "Notebook")
         ("Gtk" . "TextBuffer")
         ("Gtk" . "ScrolledWindow")
         ("GtkSource" . "View"))))



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



(define-class <gtk-webview-buffer> (<gtk-widget-buffer>
                                    <nomad-webview-buffer>
                                    <webkit-web-view>))

(define-method (initialize (self <gtk-webview-buffer>) args)
  (next-method)
  (connect self 'load-changed
           (lambda _
             (let ((percent (inexact->exact
                           (round (* 100 (!estimated-load-progress self))))))
               (slot-set! self 'name (!uri self)))
             #t))
  (buffer-load-uri self (!init-uri self)))

(define-method (buffer-load-uri (buffer <gtk-webview-buffer>) uri)
  (webkit-web-view-load-uri buffer uri))

(define-method (buffer-uri (buffer <gtk-webview-buffer>))
   (webkit-web-view-get-uri buffer))



(define-class <gtk-textview-buffer> (<nomad-text-buffer>
                                     <gtk-widget-buffer>
                                     <gtk-scrolled-window>)
  (source-view #:accessor !source-view)
  (name #:init-value "<gtk-textview-buffer>"))

(define-method (initialize (self <gtk-textview-buffer>) args)
  (next-method)
  (let ((view (make <gtk-source-view> #:editable #f)))
    (set! (!source-view self) view)
    (gtk-container-add self view)
    (g-timeout-add 50 (lambda _
                        (redisplay self)))
    (gtk-widget-show-all self)
    (gtk-widget-grab-focus view)))

(define-method (redisplay (self <gtk-textview-buffer>))
  (catch 'all
    (lambda _
      (let* ((view (!source-view self))
             (buf  (gtk-text-view-get-buffer view))
             ;; (iter (begin (gtk-text-buffer-set-text buf (buffer:buffer-string self) -1)
             ;;              (gtk-text-buffer-get-start-iter buf)))
             )
        (gtk-text-buffer-set-text buf (buffer:buffer-string self) -1)
        ;; (gtk-text-iter-forward-char iter (- (buffer:point self) 1))
        ;; (gtk-text-buffer-place-cursor buf iter)
        #t))
    (lambda (key args)
      (format #t "key: ~a arg: ~a" key args))))


