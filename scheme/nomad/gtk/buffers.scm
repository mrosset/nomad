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
  #:use-module (nomad gtk generics)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<gtk-webview-buffer>))

(eval-when (expand load eval)
  (gi-import-by-name "WebKit2" "WebView"))

(define-class <gtk-webview-buffer> (<nomad-webview-buffer> <webkit-web-view>))

(define-method (initialize (self <nomad-webview-buffer>) args)
  (next-method)
  (slot-set! self 'name "initializing...")
  (let ((uri (slot-ref self 'init-uri)))
    (connect self 'load-changed
             (lambda _
               (slot-set! self 'name (buffer-uri self))))
    (buffer-load-uri self uri))
  (add-hook! (buffer-kill-hook self)
             (lambda _
               (gtk-widget-destroy self)))
  (switch-to-buffer self))
