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

(define-module (nomad webview)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad platform)
  #:export (%search-provider-format
            %default-home-page))

(define %search-provider-format "https://duckduckgo.com/?q=~a")

(define %default-home-page "https://www.gnu.org/software/guile")

(define-interactive (current-url)
  "Returns the current url"
  (message "~a"
           (buffer-uri (current-buffer))))

(define (ensure-protocol uri)
  (if (or (string-prefix? "https://" uri)
          (string-prefix? "http://" uri))
      uri
      (string-append "https://" uri)))

(define-interactive (make-buffer #:optional (uri (read-from-minibuffer "Url: ")))
  "Creates a new webview-buffer with URL"
  (make <webview-buffer> #:init-uri  (ensure-protocol uri)))

(define-interactive (edit-uri)
  "Edit the current-url."
  (buffer-load-uri (current-buffer)
                   (read-from-minibuffer "Url: " (buffer-uri (current-buffer))))
  #t)

(define-interactive (load-uri #:optional (n (universal-argument-pop!)))
  "Loads @var{uri} with current buffer. If the current buffer is not a webview
it will create a new @var{<webview-buffer>}. When used with universal argument
@var{uri} will be loaded with a new buffer."
  (let ((uri (ensure-protocol (read-from-minibuffer "Url: "))))
    (if (or (not (is-a? <webview-buffer> (current-buffer))) (> n 1) )
        (make <webview-buffer> #:init-uri uri)
        (buffer-load-uri (current-buffer) uri))
    #t))

(define-interactive (query #:optional (n (universal-argument-pop!)))
  "Queries the default search provider @var{%search-provider-format}"
  (let ((uri (simple-format #f %search-provider-format
                               (read-from-minibuffer "Query: "))))
    (if (or (not (is-a? <webview-buffer> (current-buffer))) (> n 1) )
        (make <webview-buffer> #:init-uri uri)
        (buffer-load-uri (current-buffer) uri))
    #t))
