;; application.scm
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

(define-module (nomad application)
  #:use-module (emacsy emacsy)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad log)
  #:use-module (oop goops)
  #:export (%shutdown-hook
            %startup-hook
            app-init))

(define-public load-home-page? (make-parameter #t))

;; This is run once during initial program startup
(define %startup-hook (make-hook))

;; This run when the application is shutting down.
(define %shutdown-hook (make-hook))

(define (app-init)
  "This is called when the application is activated. Which ensures
controls are accessible to scheme"
  (when (load-home-page?)
    (make <web-buffer>)))

(add-hook! %startup-hook app-init)

(add-hook! %shutdown-hook
           (lambda _
             (log-info "Running shutdown hook")))
