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
  #:use-module (nomad webkit)
  #:export (
            default-home-page
            current-url
            search-provider-format
            browse
            forward
            home
            reload
            back
            query))

(define search-provider-format "https://duckduckgo.com/?q=~a")
(define default-home-page "https://www.gnu.org/software/guile")

;; FIXME: https prefixing is pretty dumb use something that is uri
;; aware. for example if URL has prefix of http:// already it will
;; still prefix with https://
(define (browse url)
  "Browse to URL. URL is prefixed with https:// if only a domain is
passed. Returns the final URL passed to webkit"
  (let ((prefix "https://"))
    (if (not (string-prefix? prefix url))
        (set! url (string-append prefix url)))
    (web-view-load-uri url)))

(define (forward)
  (web-view-go-forward))

(define (home)
  (web-view-load-uri default-home-page))

(define (reload)
  (web-view-reload))

(define (back)
  "go back in history"
  (run-hook event-hook "(back)")
  (web-view-go-back))

(define (query arg)
  (let ((uri (simple-format #f search-provider-format arg)))
    (browse uri)))

(define (current-url)
  (web-view-current-url))
