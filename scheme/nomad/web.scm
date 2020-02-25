;; web.scm
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

(define-module (nomad web)
  #:use-module (srfi srfi-26)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad util)
  #:use-module (nomad text)
  #:use-module (g-golf)
  #:export (%search-provider-format
            %default-home-page
            %search-providers
            %web-mode-map
            <web-buffer>
            buffer-uri
            buffer-progress
            buffer-title
            current-search))

(define %search-provider-format "https://duckduckgo.com/?q=~a")

(define %default-home-page "https://www.gnu.org/software/guile")

(define-public web-mode (make <mode> #:mode-name "web"))

;; search providers
(define %search-providers
  (circular-list "https://searx.info/?q=~a"
                 "https://google.com/?q=~a"
                 "https://duckduckgo.com/?q=~a"))

(define %web-mode-map)

(define (uniquify-name str lst)
  (define (name x)
    (if (equal? x 0)
        (format #f str "")
        (format #f str x)))

  (let loop ((n 0))
    (if (not (member (name n) lst))
        (name n)
        (loop (1+ n)))))

(define-class <web-buffer> (<widget-buffer>)
  (keymap   #:accessor     local-keymap
            #:init-keyword #:keymap
            #:init-form    %web-mode-map)
  (name     #:init-keyword #:name
            #:init-thunk  (lambda ()
                           (uniquify-name "<web-buffer: ~a>" (map buffer-name (buffer-list)))))
  (progress #:accessor     buffer-progress
            #:init-value   0)
  (title    #:accessor     buffer-title
            #:init-value   "")
  (uri      #:accessor     buffer-uri
            #:init-keyword #:uri
            #:init-value   %default-home-page)
  (search   #:accessor     current-search
            #:init-value   #f))

(define-public (make-web-buffer uri)
  (make <web-buffer> #:uri uri))

(set! buffer-classes (cons* <web-buffer>
                            buffer-classes))
