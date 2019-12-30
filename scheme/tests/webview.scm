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

(define-module (tests webview)
  #:use-module (emacsy emacsy)
  #:use-module (nomad webview)
  #:use-module (nomad application)
  #:use-module (nomad util)
  #:use-module (nomad widget)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign))

(test-begin "webview")

(import-functions "Gtk" '("init_check"))

(let ((gtk? (gtk-init-check #f #f)))
  (test-assert "Gtk init?" gtk?))

(test-equal "https prefix url" (prefix-url "127.0.0.1") "https://127.0.0.1")

(test-equal "don't prefix http" (prefix-url "http://127.0.0.1") "http://127.0.0.1")

(test-equal "don't prefix https" (prefix-url "https://127.0.0.1") "https://127.0.0.1")


;; (define-class <wildebeest> () #:metaclass <redefinable-class>)
;; (define-class <gnu> (<wildebeest>))

;; (test-assert "change class"
;;   (let ((beast (make <wildebeest>)))
;;     (change-class beast <gnu>)
;;     (eq? (class-of beast) <gnu>)))

(test-assert "change class to webview"
  (let ((buffer (make <text-buffer>)))
    (change-class buffer <webview-buffer>)
    (eq? (class-of buffer) <webview-buffer>)))

(test-group "webview class"
            (let ((buffer (make-webview-buffer "gnu.org")))
              (test-equal <webview-buffer> (class-of buffer))
              (test-equal <webview-buffer> (class-of (buffer-widget buffer)))
              (test-equal #f (current-search buffer))
              (test-equal "gnu.org" (buffer-name buffer))
              (test-equal #f (buffer-uri buffer))
              (test-equal #f (null-pointer? (buffer-pointer buffer)))
              (test-equal "https://gnu.org/"
                (begin (buffer-load-uri buffer "https://gnu.org")
                       (buffer-uri buffer)))))

(test-group "webcontent buffer"
            (let ((buffer (make-webcontent-buffer "test-content")))
              (test-equal "buffer-name"
                "test-content"
                (buffer-name buffer))
              (test-equal "buffer-url"
                #f
                (buffer-uri buffer))
              (test-equal "buffer-content"
                "<h2>test-content</h2>"
                (buffer-content buffer))
              (test-equal "buffer-pointer"
                #f
                (null-pointer? (buffer-pointer buffer)))))

(test-end)
