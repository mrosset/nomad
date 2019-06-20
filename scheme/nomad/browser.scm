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
  #:use-module (emacsy emacsy)
  #:use-module (nomad webkit)
  #:use-module (nomad eval)
  #:use-module (ice-9 optargs)
  #:export (current-url
            default-home-page
            prefix-url
            search-provider-format
            history-forward
            webview-mode-map))

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

(define-command (browse url)
  "Browse to URI. URI is prefixed with https:// if no protocol is
specified. Returns the final URL passed to webkit"
    (webview-load-uri (prefix-url url)))

(define-interactive
  (load-url #:optional
        (url (read-from-minibuffer "URL: ")))
  (message (webview-load-uri (prefix-url url))))

(define-command (forward)
  "Go forward in browser history"
  (webview-go-forward))

(define-command (home)
  "Load default home page"
  (webview-load-uri default-home-page))

(define-command (reload)
  "Reload current URI"
  (webview-reload))

(define-command (back)
  "Browse backwards in history"
  (webview-go-back))

(define-command (make-query q)
  "Makes a new buffer and queries ARG using 'search-provider-format"
  (make-buffer (simple-format #f search-provider-format q)))

(define-command (query q)
  "Queries ARG using 'search-provider-format"
  (let ((uri (simple-format #f search-provider-format q)))
    (browse uri)))

(define-interactive (current-url)
  "Returns the current url"
  (message (webview-current-url)))
