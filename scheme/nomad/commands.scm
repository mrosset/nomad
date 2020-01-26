;; commands.scm
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

(define-module (nomad commands)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad frame)
  #:use-module (nomad platform))

(define-interactive (current-url)
  "Returns the current url"
  (message "~a"
           (buffer-uri (current-buffer))))

(define-interactive (make-buffer #:optional (uri (read-from-minibuffer "Url: ")))
  "Creates a new webview-buffer with URL"
  (let ((buffer (make <gtk-webview-buffer> #:init-uri uri)))
    (buffer-load-uri buffer uri)
    buffer))

(define-interactive (toggle-tabs #:optional (frame (current-frame)))
  "Toggles the current notebook tabs on or off."
  (toggle-tabs* frame)
  #t)
