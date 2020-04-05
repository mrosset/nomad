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
  #:use-module (nomad gtk gi)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (emacsy emacsy)
  #:use-module (nomad init)
  #:use-module (nomad api)
  #:use-module (nomad web)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:use-module (nomad web)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<nomad-gtk-application>))

(define-public (current-application)
  "Returns the default application"
  (g-application-get-default))

(define-public (application-id)
  "Returns the default application id"
  (g-application-get-application-id (current-application)))



(define-class <nomad-gtk-application> (<nomad-application> <gtk-application>))

(define (initialize-extention-cb ctx)
  (webkit-web-context-set-web-extensions-directory
   ctx
   (getenv "NOMAD_WEB_EXTENSION_DIR")))

(define (startup-cb app)
  (dimfi "STARTUP")
  (emacsy-initialize #t)
  (init)
  (connect (webkit-web-context-get-default)
           'initialize-web-extensions
           initialize-extention-cb)
  ;; setup proxy
  (when (and (use-proxy?)
             (proxy-uri)
             (proxy-ignore-hosts))
    (webkit-web-context-set-network-proxy-settings
     (webkit-web-context-get-default)
     'custom
     (webkit-network-proxy-settings-new (proxy-uri)
                                        (proxy-ignore-hosts))))
  (when %use-cookies?
    (let ((manager (webkit-web-context-get-cookie-manager
                    (webkit-web-context-get-default))))
      (webkit-cookie-manager-set-persistent-storage
       manager
       %user-cookie-file
       'sqlite))))

(define (activate-cb app)
  (dimfi "ACTIVATE" (application-id))
  (if (current-frame)
      (dimfi "Nomad only supports one frame.")
      (begin
        (gtk-frame-new app)
        (run-hook (!startup-hook app)))))

(define (open-cb app files n-files hint)
  (dimfi "OPEN")
  (unless (current-frame)
    (gtk-frame-new app))
  (for-each (lambda (file)
              (make <web-buffer> #:uri (g-file-get-uri file)))
            files))

(define-method (initialize (self <nomad-gtk-application>) args)
  (next-method)
  (g-application-set-flags self '(handles-open can-override-app-id))
  (connect self 'open open-cb)
  (connect self 'startup startup-cb)
  (connect self 'activate activate-cb)
  (connect self 'shutdown (lambda (app)
                            (run-hook %shutdown-hook))))


