;; webview.scm
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

(define-module (nomad webview)
  #:use-module (emacsy emacsy)
  #:use-module (nomad buffer)
  #:use-module (g-golf)
  #:use-module (oop goops)
  #:export (<nomad-webview-buffer>
            !init-uri))



(define-class <nomad-webview-buffer> (<nomad-buffer>)
  (name #:init-value "*webview*")
  (init-uri #:accessor !init-uri
            #:init-keyword
            #:init-uri
            #:init-value "https://neutron.bufio.org"))

(define-method (initialize (self <nomad-webview-buffer>) args)
  (next-method))


