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
  #:use-module (nomad buffer)
  #:use-module (nomad eval)
  #:use-module (nomad events)
  #:use-module (nomad util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (current-url
            scroll-down
            scroll-up
            hints
            webview-init
            default-home-page
            prefix-url
            search-provider-format
            set-current-uri!
            history-forward
            webview-map
            firefox-webview-map))

;; Provides emacs like key mappings for webview-mode
(define webview-map (make-keymap))

;; Provides firefox key mappings for webview-mode. This can be set as
;; the default webview mode map by using (!set webview-map
;; firefox-webview-map) in user-init-file
(define firefox-webview-map (make-keymap))

(define search-provider-format "https://duckduckgo.com/?q=~a")

(define default-home-page "https://www.gnu.org/software/guile")

(define (prefix-url url)
  "Returns a full protocol URI for domain URI.
e.g. (prefix-url \"gnu.org\") returns \"https://gnu.org\""
  (if (string-contains url "://") url
      (string-append "https://" url)))

(define* (history-forward #:optional x)
  (if (not x) (webview-go-forward)
      (cond ((zero? x) #f)
            ((positive? x)
             (and (webview-go-forward)
                  (history-forward (1- x))))
            ((negative? x)
             (and (webview-go-back)
                  (history-forward (1+ x)))))))

(define (set-current-uri! uri)
  (set-buffer-uri! (current-buffer) uri))

(define-interactive (browse #:optional (url (read-from-minibuffer "URL: ")))
  "Browse to URI. URI is prefixed with https:// if no protocol is
specified. Returns the final URL passed to webkit"
  (set-current-uri! (prefix-url url)))

(define-interactive (forward)
  "Go forward in browser history"
  (webview-go-forward))

(define-interactive (home)
  "Load default home page"
  (webview-load-uri default-home-page))

(define-interactive (reload)
  "Reload current URI"
  (webview-reload))

(define-interactive (back)
  "Browse backwards in history"
  (webview-go-back))

(define-interactive (make-query #:optional (q (read-from-minibuffer "Query: ")))
  "Makes a new buffer and queries ARG using 'search-provider-format"
  (make-buffer (simple-format #f search-provider-format q)))

(define-interactive (query #:optional (q (read-from-minibuffer "Query: ")))
  "Queries ARG using 'search-provider-format"
  (let ((uri (simple-format #f search-provider-format q)))
    (browse uri)))

(define-interactive (current-url)
  "Returns the current url"
  (message "~a"
           (webview-current-url)))

(define-interactive (copy-current-url)
  "Copy current url to clipboard"
  (yank-string (webview-current-url))
  (message (webview-current-url)))

(define-interactive (load-content #:optional (content (read-from-minibuffer "Content: ")))
  "Load CONTENT into current buffer"
  (set-buffer-content! (current-buffer) content "nomad://"))

(define-public (tweak-url)
  "Edit the current-url."
  (browse (read-from-minibuffer "Url: " (current-url))))

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

(define (firefox-webview-map-init)
  "Initializes firefox-webview-map"
  ;; webview
  (define-key firefox-webview-map (kbd "C-u") 'next-buffer)
  (define-key firefox-webview-map (kbd "C-m") 'prev-buffer)
  (define-key firefox-webview-map (kbd "M-n") 'forward)
  (define-key firefox-webview-map (kbd "M-b") 'back)
  (define-key firefox-webview-map (kbd "M-h") 'home)
  (define-key firefox-webview-map (kbd "M-f") 'browse)
  (define-key firefox-webview-map (kbd "M-g") 'reload)
  (define-key firefox-webview-map (kbd "M-u") 'tweak-url)
  (define-key firefox-webview-map (kbd "M-c") 'copy-current-url)
  (define-key firefox-webview-map (kbd "C-s") 'query)
  (define-key firefox-webview-map (kbd "M-s") 'cycle-search-provider)
  (define-key firefox-webview-map (kbd "M-v") 'scroll-up)
  (define-key firefox-webview-map (kbd "C-v") 'scroll-down)
  (define-key firefox-webview-map (kbd "M-'") 'hints))

(define-public webview-map
  (list->keymap `(("C-u" back)
                  ("C-m" forward)
                  ("C-n" scroll-down)
                  ("C-p" scroll-up)
                  ("C-f" hints)
                  ("C-r" reload)
                  ("C-q" kill-buffer)
                  ("C-x C-f" query))))

(define (webview-init)
  (firefox-webview-map-init))
