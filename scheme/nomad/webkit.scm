;; webkit.scm
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
(define-module (nomad webkit)
  #:use-module (nomad lib))

(load-extension (dynamic-path) "init_guile_nomad_webkit")

(define* (proxy-set! choice #:optional proxy-settings)
  "Set Nomad default frame proxy, if choice is #t then use default system proxy.
If optional proxy-settings is present(a pointer to proxy-settings) use it as
the proxy. Otherwise if bool is #f Disable proxy."
  (if choice
      (if proxy-settings
          (webkit-set-proxy-settings-custom proxy-settings)
          (webkit-set-proxy-settings-default))
      (webkit-set-proxy-settings-no-proxy)))
(export proxy-set!)
