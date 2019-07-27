;; webkit.scm FFI for webkit
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
  #:use-module (system foreign)
  #:use-module (srfi srfi-64))

;; FIXME: this is a dirty hack. fix this so that guix can find the webkit
;; dynamic-link library. or tests fails with the following error
;;
;; ERROR: In procedure dynamic-link:
;; In procedure dynamic-link: file: "libwebkit2gtk-4.0", message: "file not found"

(if (string= (getenv "HOME")
             "/homeless-shelter")
    (test-skip "webkit")
    (use-modules (nomad webkit)))

(test-group "webkit "
            (let* ((view (webkit-new)))
              (test-assert "webview is pointer?"
                (pointer? view))
              (test-assert "view is not null"
                (not (null-pointer? view)))
              (test-equal "no uri"
                "NULL"
                (webkit-uri view))
              (test-equal "load uri"
                "https://gnu.org/"
                (begin (webkit-load-uri view "https://gnu.org")
                       (webkit-uri view)))))
