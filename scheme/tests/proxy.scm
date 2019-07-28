;; proxy.scm --- Test scheme bindings to webkit network proxy settings.

;; Copyright (C) 2019 Amar Singh<nly@disroot.org>

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

(define-module (tests proxy)
  #:use-module (nomad webkit)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign))

(test-begin "proxy")

(define proxy (webkit_network_proxy_settings_new "http://localhost:9050"
                                                 '("192.168.1.1")))

(test-equal (pointer? proxy) #t)

(webkit_network_proxy_settings_add_proxy_for_scheme proxy
                                                       "google.com" "http://localhost:9050")

(define proxy-copy (webkit_network_proxy_settings_copy proxy))

(test-equal (pointer? proxy-copy) #t)

(webkit_network_proxy_settings_free proxy)

(webkit_network_proxy_settings_free proxy-copy)

(test-end)
