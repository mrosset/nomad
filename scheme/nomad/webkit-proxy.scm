;; webkit-proxy --- configure webkit network settings.

;; Copyright (C) 2019  Amar Singh

;; This file is part of Nomad.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nomad webkit-proxy)
  #:use-module (nomad lib)
  #:export (webkit-proxy-new
            webkit-proxy-set!))

(load-extension (dynamic-path) "init_guile_nomad_webkitproxy")

(define (webkit-proxy-new proxy ignore-urls)
  "Create a new network-proxy-setting with proxy as the proxy-url and
ignore-urls and return a pointer to it."
  (webkit-proxy-settings-new proxy ignore-urls))

(define* (webkit-proxy-set! choice #:optional proxy-settings)
  "Set default Nomad proxy, if choice is #t then use default system proxy.
If optional proxy-settings is present(a pointer to proxy-settings) use it as
the proxy. Otherwise if bool is #f disable proxy."
  (if choice
      (if proxy-settings
          (webkit-set-proxy-settings-custom proxy-settings)
          (webkit-set-proxy-settings-default))
      (webkit-set-proxy-settings-no-proxy)))

