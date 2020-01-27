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

(define-module (nomad webview)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 optargs)
  #:use-module (nomad buffer)
  #:use-module (nomad frame)
  #:use-module (nomad eval)
  #:use-module (nomad widget)
  #:use-module (nomad util)
  #:use-module (g-golf)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (scroll-down
            scroll-up
            hints
            webview-init
            default-home-page
            prefix-url
            search-provider-format
            webview-map
            firefox-webview-map
            webview-onload
            ;; class constructors
            make-webview-buffer
            make-webcontent-buffer
            ;;methods
            set-buffer-hooks!
            buffer-back
            buffer-foward
            buffer-content
            buffer-render
            buffer-sync
            buffer-uri
            buffer-load-uri
            current-search
            ))

(eval-when (expand load eval)
  (gi-import "WebKit2")
  (gi-import-by-name "Gtk" "Widget"))

;;; <webview-buffer> inherits <text-buffer> and <nomad-web-view>
(define-class <webview-buffer>
  (<text-buffer> <nomad-web-view>)
  (content #:accessor buffer-content #:init-keyword #:content)
  (search #:accessor current-search #:init-keyword #:current-search #:init-value #f))

(define-method (buffer-pointer (buffer <webview-buffer>))
  (slot-ref buffer 'g-inst))

(define-method (buffer-widget (buffer <webview-buffer>))
  buffer)

(export <webview-buffer> buffer-widget)

(define-method (buffer-reload)
  (webkit-web-view-reload (current-buffer)))

(define-method (buffer-hints)
  (webkit-hints (buffer-pointer (current-buffer))))

(define-method (buffer-scroll-up)
  (webkit-scroll-up (buffer-pointer (current-buffer))))

(define-method (buffer-scroll-down)
  (webkit-scroll-down (buffer-pointer (current-buffer))))

(define-method (buffer-back (buffer <webview-buffer>))
  (webkit-web-view-go-back buffer))

(define-method (buffer-back)
  (buffer-back (current-buffer)))

(define-method (buffer-forward)
  (buffer-forward (current-buffer)))

(define-method (buffer-forward (buffer <webview-buffer>))
  (webkit-web-view-go-forward buffer))

(define (webview-kill-hook)
  (info "Destroying pointer ~a"
        (buffer-pointer))
  (gtk-widget-destroy (current-buffer)))

(define (webview-enter-hook)
  (info "Setting widget to ~a"
        (current-buffer))
  (switch-to-buffer-widget (current-buffer))
  (gtk-widget-grab-focus (current-buffer)))

(define-method (set-buffer-hooks!)
  (set-buffer-hooks! (current-buffer)))

(define-method (set-buffer-hooks! (buffer <webview-buffer>))
  (add-hook! (buffer-enter-hook buffer)
             webview-enter-hook)
  (add-hook! (buffer-kill-hook buffer)
             webview-kill-hook))

(define (webview-onload)
  "Update BUFFER on webview load"
  (set-buffer-name! (buffer-uri (current-buffer))))

(define-method (buffer-uri)
  (buffer-uri (current-buffer)))

(define-method (buffer-uri (buffer <webview-buffer>))
  (webkit-web-view-get-uri buffer))

(define-method (buffer-load-uri uri)
  (buffer-load-uri (current-buffer) uri))

(define-method (buffer-load-uri (buffer <webview-buffer>)
                                       uri)
  (webkit-web-view-load-uri buffer uri))

(define-method (buffer-render)
  (buffer-render (current-buffer)))

(define-method (buffer-render (buffer <webview-buffer>))
  (webkit-web-view-load-html buffer (buffer-content buffer) "nomad://")
  #t)

(define* (make-webview-buffer #:optional (uri default-home-page))
  "Constructs a new webview-buffer class"
  (let ((buffer (make <webview-buffer> #:name uri #:uri uri #:keymap webview-map)))
    (add-buffer! buffer)
    buffer))

(define* (make-webcontent-buffer name
                                 #:optional (content (format #f "<h2>~a</h2>" name)))
  "Constructs a new webcontent-buffer class"
  (let ((buffer (make <webview-buffer>
                  #:name name #:content content
                  #:keymap webview-map)))
    (add-buffer! buffer)
    buffer))

(define search-provider-format "https://duckduckgo.com/?q=~a")

(define default-home-page "https://www.gnu.org/software/guile")

(define (prefix-url url)
  "Returns a full protocol URI for domain URI.
e.g. (prefix-url \"gnu.org\") returns \"https://gnu.org\""
  (if (string-contains url "://") url
      (string-append "https://" url)))

(define-interactive (browse #:optional (url (read-from-minibuffer "URL: ")))
  "Browse to URI. URI is prefixed with https:// if no protocol is
specified. Returns the final URL passed to webkit"
  (buffer-load-uri (current-buffer)
                   (prefix-url url)))

(define-interactive (hints)
  (buffer-hints))

(define-interactive (scroll-up)
  "Scroll buffer up"
  (buffer-scroll-up))

(define-interactive (scroll-down)
  "Scroll buffer down"
  (buffer-scroll-down))

(define-interactive (forward)
  "Go forward in browser history"
  (buffer-forward))

(define-interactive (back)
  "Browse backwards in history"
  (buffer-back))

(define-interactive (home)
  "Load default home page"
  (buffer-load-uri default-home-page))

(define-interactive (reload)
  "Reload current URI"
  (buffer-reload)
  #t)

(define-interactive (make-query #:optional (q (read-from-minibuffer "Query: ")))
  "Makes a new buffer and queries ARG using 'search-provider-format"
  (make-buffer (simple-format #f search-provider-format q)))

(define-interactive (query #:optional (q (read-from-minibuffer "Query: ")))
  "Queries ARG using 'search-provider-format"
  (let ((uri (simple-format #f search-provider-format q)))
    (browse uri)))

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

;; search providers
(define search-providers
  (circular-list "https://searx.info/?q=~a"
                 "https://google.com/?q=~a"
                 "https://duckduckgo.com/?q=~a"))

(define (pick-search-provider)
  (let ((s search-providers))
    (lambda ()
      (set! search-provider-format (car s))
      (set! s (cdr s)))))

(define-public cycle-search-provider (pick-search-provider))

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
(define firefox-webview-map
  (list->keymap '(("C-g" webview-keyboard-quit)
                  ("C-f" isearch-forward)
                  ("C-u" next-buffer)
                  ("C-m" prev-buffer)
                  ("M-n" forward)
                  ("M-b" back)
                  ("M-h" home)
                  ("M-f" browse)
                  ("M-g" reload)
                  ("M-u" tweak-url)
                  ("M-c" copy-current-url)
                  ("C-s" query)
                  ("M-s" cycle-search-provider)
                  ("M-v" scroll-up)
                  ("C-v" scroll-down)
                  ("M-'" hints))))

;; Default webview key mappings
(define-public webview-map
  (list->keymap '(("C-u" back)
                  ("C-m" forward)
                  ("C-n" scroll-down)
                  ("C-p" scroll-up)
                  ("C-f" hints)
                  ("C-r" reload)
                  ("C-g" webview-keyboard-quit)
                  ("C-s" isearch-forward))))
