;; webkit.scm - provides testing FFI to gtk webkit2
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
  #:use-module (system foreign)
  #:export (webkit-new
            webkit-uri
            webkit-load-uri
            gtk-destroy))

(define libwebkit (dynamic-link "libwebkit2gtk-4.0"))
(define libgtk (dynamic-link "libgtk-3"))

;; gtk destroy widget procedure
(define gtk-destroy
  (pointer->procedure void
                      (dynamic-func "gtk_widget_destroy" libgtk)
                      '(*)))

;;; webview constructor procedure
(define webkit-new
  (pointer->procedure '*
                      (dynamic-func "webkit_web_view_new" libwebkit)
                      '()))

(define (webkit-uri view)
  "Return VIEW's uri"
  (let* ((proc (pointer->procedure '*
                                   (dynamic-func "webkit_web_view_get_uri" libwebkit)
                                   '(*)))
         (return-p (proc view)))
    (if (null-pointer? return-p)
        #f
        (pointer->string return-p))))

(define (webkit-load-uri view uri)
  "Sets VIEW's uri"
  (let ((proc (pointer->procedure void
                                      (dynamic-func "webkit_web_view_load_uri" libwebkit)
                                      '(* *))))
    (proc view
              (string->pointer uri))))
