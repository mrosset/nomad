 ;; web-mode.scm
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

(define-module (nomad web-mode)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad platform)
  #:use-module (nomad util)
  #:use-module (nomad web))

(define-interactive (current-url)
  "Returns the current url"
  (message "~a"
           (buffer-uri (current-buffer))))

(define (ensure-protocol url)
  "Returns a full protocol URI for domain URI.
e.g. (prefix-url \"gnu.org\") returns \"https://gnu.org\""
  (if (string-contains url "://")
      url
      (string-append "https://" url)))

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

(define-interactive (load-uri #:optional
                              (buffer (current-buffer))
                              (uri (ensure-protocol (read-from-minibuffer "Url: "))))
  "Loads @var{uri} with current buffer.  If the current buffer is not a webview
it will create a new @var{<web-buffer>}.  When used with universal argument
@var{uri} will be loaded with a new buffer."
  (if (or (> (universal-argument-pop!) 1)
          (not (is-a? (current-buffer) <web-buffer>)))
      (make <web-buffer> #:uri uri)
      (buffer-load-uri (current-buffer) uri))
  #t)

(define-interactive (load-clipboard-uri)
  (load-uri (current-buffer) (get-clipboard)))

(define-interactive (make-buffer-clipboard)
  (make <web-buffer> #:uri (get-clipboard))
  #t)

(define-interactive (hints #:optional (buffer (current-buffer)))
  (buffer-hints buffer)
  #t)

(define-interactive (scroll-up #:optional (buffer (current-buffer)))
  "Scroll buffer up"
  (buffer-scroll-up buffer)
  #t)

(define-interactive (scroll-down #:optional (buffer (current-buffer)))
  "Scroll buffer down"
  (buffer-scroll-down buffer)
  #t)

(define-interactive (forward #:optional (buffer (current-buffer)))
  "Go backwards in history"
  (buffer-forward buffer)
  #t)

(define-interactive (back #:optional (buffer (current-buffer)))
  "Browse backwards in history"
  (buffer-back buffer)
  #t)

(define-interactive (home #:optional (buffer (current-buffer)))
  (if (is-a? buffer <web-buffer>)
      (buffer-load-uri buffer  %default-home-page)
      (make <web-buffer> #:uri %default-home-page #:name "*Home Page*"))
  #t)

(define-interactive (reload #:optional (buffer (current-buffer)))
  "Reload current URI"
  (buffer-reload buffer)
  #t)

(define-interactive (copy-current-url)
  "Copy current url to clipboard"
  (copy-text (current-url))
  (message (current-url))
  #t)

(define-interactive (isearch-forward
                     #:optional (text (or (current-search (current-buffer))
                                          (read-from-minibuffer "I-search: "))))
  (set! (current-search (current-buffer)) text)
  (search-forward (current-buffer))
  #t)

(define-interactive (web-keyboard-quit)
  (hints-finish (current-buffer))
  (search-finish (current-buffer))
  (keyboard-quit)
  #t)

(define-interactive (query #:optional
                           (text (read-from-minibuffer "Query: ")))
  "Queries the default search provider @var{%search-provider-format}.  If the
current buffer is not a @var{<web-buffer>} it will create a new
@var{<web-buffer>} and query the default search provider."
  (let ((u   (universal-argument-pop!))
        (uri (format #f
                     %search-provider-format
                     text)))
    (if (or (> u 1)
            (not (is-a? (current-buffer) <web-buffer>)))
      (make <web-buffer> #:uri uri)
      (buffer-load-uri (current-buffer) uri)))
  #t)

;; Provides firefox key mappings for webview-mode. This can be set as
;; the default webview mode map by using (!set %webview-map
;; %firefox-webview-map) in %user-init-file
(define-public %firefox-webview-map
  (list->keymap '(("C-g" web-keyboard-quit)
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

(set! %web-mode-map
      (list->keymap '(("C-c u" back)
                      ("C-k" kill-buffer)
                      ("C-m" forward)
                      ("C-n" scroll-down)
                      ("C-p" scroll-up)
                      ("C-f" hints)
                      ("C-y" make-buffer-clipboard)
                      ("C-r" reload)
                      ("C-g" web-keyboard-quit)
                      ("C-s" isearch-forward))))
