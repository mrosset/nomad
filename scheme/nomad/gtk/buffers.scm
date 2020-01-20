;; buffers.scm
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

(define-module (nomad gtk buffers)
  #:use-module (emacsy emacsy)
  #:use-module (nomad buffer)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk generics)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<gtk-webview-buffer>))

(eval-when (expand load eval)
  (gi-import-by-name "WebKit2" "WebView")
  (gi-import-by-name "Gtk" "Widget")
  (gi-import-by-name "Gtk" "Notebook"))

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
             (let ((whole (inexact->exact
                           (round (* 100 (!estimated-load-progress self))))))
               (slot-set! self 'name (!uri self)))
             #t)))

(define-interactive (test-buffer)
  (make <gtk-webview-buffer> #:init-uri "https://google.ca"))
