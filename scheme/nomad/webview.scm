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
  #:use-module (nomad util)
  #:use-module (nomad platform)
  #:use-module (nomad api)
  #:use-module (srfi srfi-1)
  #:export (%search-provider-format
            %search-providers
            %default-home-page))

(define %search-provider-format "https://duckduckgo.com/?q=~a")

(define %default-home-page "https://www.gnu.org/software/guile")

;; search providers
(define %search-providers
  (circular-list "https://searx.info/?q=~a"
                 "https://google.com/?q=~a"
                 "https://duckduckgo.com/?q=~a"))

(define-interactive (current-url)
  "Returns the current url"
  (message "~a"
           (buffer-uri (current-buffer))))

(define (ensure-protocol uri)
  (if (or (string-prefix? "https://" uri)
          (string-prefix? "http://" uri))
      uri
      (string-append "https://" uri)))

(define (pick-search-provider)
  (let ((s %search-providers))
    (lambda ()
      (set! %search-provider-format (car s))
      (set! s (cdr s)))))

(define-public cycle-search-provider (pick-search-provider))

(define-interactive (edit-uri)
  "Edit the current-url."
  (buffer-load-uri (current-buffer)
                   (read-from-minibuffer "Url: " (buffer-uri (current-buffer))))
  #t)

(define-interactive (hints)
  (buffer-hints)
  #t)

(define-interactive (scroll-up)
  "Scroll buffer up"
  (buffer-scroll-up)
  #t)

(define-interactive (scroll-down)
  "Scroll buffer down"
  (buffer-scroll-down)
  #t)

(define-interactive (forward #:optional (buffer (current-buffer)))
  "Go backwards in history"
  (buffer-forward buffer)
  #t)

(define-interactive (back #:optional (buffer (current-buffer)))
  "Browse backwards in history"
  (buffer-back buffer)
  #t)

(define-interactive (home)
  (buffer-load-uri (current-buffer) %default-home-page)
  #t)

(define-interactive (reload #:optional (buffer (current-buffer)))
  "Reload current URI"
  (buffer-reload buffer)
  #t)

(define-interactive (copy-current-url)
  "Copy current url to clipboard"
  (yank-string (current-url))
  (message (webview-current-url)))

(define-interactive
  (isearch-forward #:optional
                   (text (or (current-search (current-buffer)) (read-from-minibuffer "I-search: "))))
  (slot-set! (current-buffer) 'search text)
  (let ((controller (webkit-web-view-get-find-controller (current-buffer))))
    (webkit-find-controller-search controller text 0 255)
    (message "I-search: ~a" text)))

(define-interactive (load-uri #:optional
                              (uri (ensure-protocol (read-from-minibuffer "Url: ")))
                              (buffer (current-buffer)))
  "Loads @var{uri} with current buffer. If the current buffer is not a webview
it will create a new @var{<webview-buffer>}. When used with universal argument
@var{uri} will be loaded with a new buffer."
  (buffer-load-uri buffer uri)
  #t)

(define-public is-a is-a?)

(define-interactive (query #:optional (n (universal-argument-pop!)))
  "Queries the default search provider @var{%search-provider-format}"
  (let ((uri (simple-format #f %search-provider-format
                               (read-from-minibuffer "Query: "))))
    (if (or (not (is-a? <webview-buffer> (current-buffer))) (> n 1) )
        (make <webview-buffer> #:init-uri uri)
        (buffer-load-uri (current-buffer) uri))
    #t))

(define-interactive (webview-keyboard-quit)
  (when (current-search (current-buffer))
    (let ((controller (webkit-web-view-get-find-controller (current-buffer))))
      (slot-set! (current-buffer)
                 'search
                 #f)
      (webkit-find-controller-search-finish controller)))
    (keyboard-quit))

;; Provides firefox key mappings for webview-mode. This can be set as
;; the default webview mode map by using (!set webview-map
;; firefox-webview-map) in user-init-file
(define-public %firefox-webview-map
  (list->keymap '(("C-g" webview-keyboard-quit)
                  ("C-f" isearch-forward)
                  ("C-u" next-buffer)
                  ("C-m" prev-buffer)
                  ("M-n" forward)
                  ("M-b" back)
                  ("M-h" home)
                  ("M-f" load-uri)
                  ("M-g" reload)
                  ("M-u" edit-uri)
                  ("M-c" copy-current-url)
                  ("C-s" query)
                  ("M-s" cycle-search-provider)
                  ("M-v" scroll-up)
                  ("C-v" scroll-down)
                  ("M-'" hints))))

;; Default webview key mappings
(set! %webview-map
  (list->keymap '(("C-u" back)
                  ("C-m" forward)
                  ("C-n" scroll-down)
                  ("C-p" scroll-up)
                  ("C-f" hints)
                  ("C-r" reload)
                  ("C-g" webview-keyboard-quit)
                  ("C-s" isearch-forward))))
