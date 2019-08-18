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

(define-module (test webkit)
  #:use-module (g-golf)
  #:use-module (nomad util)
  #:use-module (srfi srfi-64))

(gi-import "WebKit2")

(import-functions "Gtk" '("init_check"))

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "Gtk init?" gtk?))

(test-group "webkit"
            (let* ((settings (webkit-network-proxy-settings-new "http://thou.shall.not.pass:8080"
                                                                '("*.gnu.org")))
                   (view (make <webkit-web-view>))
                   (context (webkit-web-context-get-default)))

              (test-assert (not (unspecified? context)))

              (webkit-web-context-set-network-proxy-settings context 'custom settings)
              ))
