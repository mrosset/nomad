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
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (emacsy emacsy)
  #:use-module (nomad init)
  #:use-module (nomad api)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:export (<nomad-gtk-application>))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (gi-import "Gio")
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("Gtk" . "Widget")
         ("Gtk" . "Application")
         ("WebKit2" . "WebContext")
         ("WebKit2" . "CookieManager"))))

(define-public (application-id)
  "Returns the default application id"
  (let ((app (g-application-get-default)))
    (g-application-get-application-id app)))



(define-class <nomad-gtk-application> (<nomad-application> <gtk-application>))

(define (initialize-extention-cb ctx)
  (webkit-web-context-set-web-extensions-directory
   ctx
   (getenv "NOMAD_WEB_EXTENTION_DIR")))

(define (startup-cb app)
  (emacsy-initialize #t)
  (when %use-cookies?
    (let ((manager (webkit-web-context-get-cookie-manager
                    (webkit-web-context-get-default))))
      (webkit-cookie-manager-set-persistent-storage
       manager
       %user-cookie-file
       'sqlite))))

(define (activate-cb app)
  (gtk-widget-show-all (gtk-frame-new app))
  (run-hook (!startup-hook app)))

(define-method (initialize (self <nomad-gtk-application>) args)
  (next-method)
  (connect self 'startup startup-cb)
  (connect self 'activate activate-cb)
  (connect (webkit-web-context-get-default)
           'initialize-web-extensions
           initialize-extention-cb))
