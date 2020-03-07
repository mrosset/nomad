;; proxy.scm --- Proxy configuration

;; Copyright (C) 2020 by Amar M. Singh

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

(define-module (nomad proxy)
  #:export (
            %use-proxy?
            %proxy-uri
            %proxy-ignore-hosts
            make-proxy
            buffer-proxy-set!
            ))

(define %use-proxy? (make-parameter #f))
(define %http-proxy (make-parameter (getenv "HTTP_PROXY")))
(define %proxy-uri (make-parameter (or (%http-proxy))))
(define %proxy-ignore-hosts (make-parameter #f))

(define (make-proxy proxy-url ignore-list)
  (webkit-network-proxy-settings-new proxy-url ignore-list))

(define (buffer-proxy-set! buffer proxy)
  (let ((widget (buffer-widget buffer)))
    (if widget
        (webkit-web-context-set-network-proxy-settings
         (buffer-widget buffer)
         'custom
         proxy))))
