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
  #:use-module (nomad webview)
  #:use-module (srfi srfi-64))

(test-begin "webview")

(test-equal "https prefix url" (prefix-url "127.0.0.1") "https://127.0.0.1")

(test-equal "don't prefix http" (prefix-url "http://127.0.0.1") "http://127.0.0.1")

(test-equal "don't prefix https" (prefix-url "https://127.0.0.1") "https://127.0.0.1")

(test-end)
