;; api.scm
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

(define-module (nomad api)
  #:use-module (nomad init)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module (g-golf)
  #:export (<nomad-application>
            !startup-hook
            <nomad-frame>
            webview-mode))



(define-class <nomad-application> ()
  (startup-hook #:accessor !startup-hook
                #:init-keyword #:startup-hook
                #:init-value %startup-hook))

(define-method (initialize (self <nomad-application>) args)
  (next-method)
  ;; (init)
  )



(define-class <nomad-frame> ())


