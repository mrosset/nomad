;; web.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad util)
  #:use-module (nomad text)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (%search-provider-format
            %default-home-page
            %search-providers
            %web-mode-map
            %styles
            %default-style
            %default-web-settings
            %load-committed-hook
            <web-buffer>
            buffer-uri
            buffer-progress
            buffer-title
            use-proxy?
            http-proxy
            proxy-uri
            proxy-ignore-hosts
            current-search))

(define %search-provider-format "https://duckduckgo.com/?q=~a")

(define %default-home-page "nomad:")

(define %load-committed-hook (make-hook 1))

(define-public web-mode (make <mode> #:mode-name "Web"))

;; search providers
(define %search-providers
  (circular-list "https://searx.info/?q=~a"
                 "https://google.com/?q=~a"
                 "https://duckduckgo.com/?q=~a"))

(define %web-mode-map)

(define %styles #f)

(define %default-style #f)

(define %default-web-settings (make-parameter #f))

(define-class <web-buffer> (<widget-buffer>)
  (keymap   #:accessor     local-keymap
            #:init-keyword #:keymap
            #:init-form    %web-mode-map)
  (name     #:init-keyword #:name
            #:init-form    (uniquify-name "*web-buffer*<~a>"
                                          (map buffer-name (buffer-list))))
  (buffer-modes #:accessor buffer-modes #:init-value `(,web-mode))
  (progress #:accessor     buffer-progress
            #:init-value   0)
  (title    #:accessor     buffer-title
            #:init-value   "")
  (uri      #:accessor     buffer-uri
            #:init-keyword #:uri
            #:init-form    %default-home-page)
  (search   #:accessor     current-search
            #:init-value   #f))

(set! buffer-classes (cons <web-buffer>
                            buffer-classes))

(define-method (buffer-uri (buffer <buffer>))
  (message "~a has no 'buffer-uri method"
           (class-name (class-of buffer))))

(define use-proxy? (make-parameter #f))
(define http-proxy (make-parameter (getenv "HTTP_PROXY")))
(define proxy-uri (make-parameter (or (http-proxy))))
(define proxy-ignore-hosts (make-parameter '()))
