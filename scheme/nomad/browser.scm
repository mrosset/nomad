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
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 optargs)
  #:use-module (nomad buffer)
  #:use-module (nomad eval)
  #:use-module (nomad events)
  #:use-module (nomad util)
  #:use-module (nomad webkit)
  #:export (current-url
            default-home-page
            prefix-url
            search-provider-format
            history-forward
            webview-mode-map
            webview-map))

(define webview-mode-map '(("C-b" . (next-buffer))
                                  ("C-u" . (back))
                                  ("C-m" . (forward))
                                  ("C-n" . (scroll-down))
                                  ("C-f" . (hints))
                                  ("C-p" . (scroll-up))
                                  ("C-r" . (reload))
                                  ("C-x" . (kill-buffer))))

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

(define-interactive (browse #:optional (url (read-from-minibuffer "URL: ")))
  "Browse to URI. URI is prefixed with https:// if no protocol is
specified. Returns the final URL passed to webkit"
  (webview-load-uri (prefix-url url)))

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
  "Load CONTENT into current webview"
  (webview-load-content content "nomad://"))

;; FIXME: convert these to a mode-map
(define webview-map (make-keymap))
(define-key webview-map (kbd "C-u") 'next-buffer)
(define-key webview-map (kbd "C-m") 'prev-buffer)
