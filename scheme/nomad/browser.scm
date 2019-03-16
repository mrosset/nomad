;; browser.scm
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

(define-module (nomad browser)
  #:use-module (nomad events)
  #:use-module (nomad buffer)
  #:use-module (nomad webview)
  #:export (back
            browse
            current-url
            default-home-page
            forward
            home
            make-query
            prefix-url
            query
            reload
            search-provider-format))

(define search-provider-format "https://duckduckgo.com/?q=~a")
(define default-home-page "https://www.gnu.org/software/guile")

(define (prefix-url url)
  "Returns a full protocol URI for domain URI.
e.g. (prefix-url \"gnu.org\") returns \"https://gnu.org\""

  (and (not (string-prefix? "http://" url))
       (not (string-prefix? "https://" url))
       (set! url (string-append "https://" url)))
  url)

(define (browse url)
  "Browse to URI. URI is prefixed with https:// if no protocol is
specified. Returns the final URL passed to webkit"
    (webview-load-uri (prefix-url url)))

(define (forward)
  (webview-go-forward))

(define (home)
  (webview-load-uri default-home-page))

(define (reload)
  (webview-reload))

(define (back)
  "go back in history"
  (run-hook event-hook "(back)")
  (webview-go-back))

(define (make-query arg)
  "Makes a new buffer and queries ARG using 'search-provider-format"
  (make-buffer (simple-format #f search-provider-format arg)))

(define (query arg)
  "Queries ARG using 'search-provider-format"
  (let ((uri (simple-format #f search-provider-format arg)))
    (browse uri)))

(define (current-url)
  (webview-current-url))
