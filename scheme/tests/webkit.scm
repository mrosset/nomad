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
  #:use-module (srfi srfi-64))

(gi-import "Gtk")
(gi-import "WebKit2")

(test-skip "webkit")

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "Gtk init?" gtk?))

(test-group "webkit"
            (let* ((settings (webkit-network-proxy-settings-new "localhost:8080"
                                                                '("*.gnu.org")))
                   (view (make <web-kit-web-view>))
                   (view-context (webkit-web-view-get-context view))
                   (global-context (webkit-web-context-get-default))
                   (new-context (make <web-kit-web-context>)))

              (test-assert (not (null? proxy-settings))) ;; Fails because its '()
              (test-assert (not (unspecified? view-context))) ;; Fails because it's unspecified
              (test-assert (not (unspecified? global-context))) ;; Fails because it's unspecified
              (test-assert (not (unspecified? new-context)))    ;; Passes
              (test-equal <web-kit-web-context> (class-of view-context)) ;; Fails
              (test-equal <web-kit-web-context> global-context)          ;; Fails
              (test-equal <web-kit-web-context> (class-of new-context))  ;; Passes

              ;; setting the proxy setting fails with
              ;; In procedure sizeof: Wrong type argument in position 1: ()
              ;;
              ;; empty struct seems to be the culprit here
              (webkit-web-context-set-network-proxy-settings new-context 'custom settings)
   ))
