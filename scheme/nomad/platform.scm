;; platform.scm
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

(define-module (nomad platform)
  #:use-module (nomad util)
  #:use-module (nomad gtk gtk)
  #:use-module (emacsy emacsy)
  #:use-module (nomad api)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (platform-classes))

(define platform-classes '(<textview-buffer>
                           <popup-buffer>
                           <webview-buffer>
                           <nomad-gtk-window>
                           <application>
                           <frame>))

(define-interactive (describe-class
                     #:optional  (class (completing-read "Class: " (map symbol->string platform-classes))))
  (describe (primitive-eval (string->symbol class))))

(define-interactive (make-webview-buffer
                     #:optional (uri (read-from-minibuffer "Url: ")))
  (make <webview-buffer> #:init-uri uri))

(define-interactive (make-frame #:optional (app (current-application)))
  (gtk-frame-new app)
  (switch-to-buffer (next-buffer)))

(re-export-modules
 '(nomad gtk gtk))
