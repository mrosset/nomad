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
  #:use-module (nomad webkit)
  #:use-module (nomad eval)
  #:use-module (ice-9 optargs)
  #:export (current-url
            default-home-page
            prefix-url
            search-provider-format
            history-forward))

(define-public webview-mode-map '(("C-b" . (next-buffer))
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

(define-command (current-url)
  "Returns the current url"
  (webview-current-url))

;; Nomad feature API cum wishlist
;; Icecat
;; adblocker/js-blocker
(use-modules (ice-9 optargs)
             (srfi srfi-1)
             (srfi srfi-9))
(define* (javascript-enable bool #:optional buffer)
  "Toggel Javascript in current buffer.
Or if provided in BUFFER"
  #f)

;; proxy:
(define* (proxy-server #:optional scheme+uri)
  "Return current proxy-server, if provided Set scheme+uri as the new
proxy-server."
  #f)
;; fonts:
(define* (font #:optional font-name)
  "Return font-name, if FONT-NAME is provided, set font to
FONT-NAME."
  #f)
;; fullscreen:
(define* (fullscreen #:optional boolean)
  "Return fullscreen status, if BOOLEAN set fullscreen to BOOLEAN."
  #f)
;; page-down:
(define* (scroll-page integer #:optional goto-end?)
  "Scroll page by INTEGER no. of pages, negative values mean scroll
upwards. If goto-end? then go to beginning if positive, or go to end
if negative INTEGER."
  #f)
;; move-frame
(define* (switch-buffer buffer #:optional tab)
  "Switch to buffer BUFFER in current, or if present TAB."
  #f)
;; save-page:
(define* (save-page #:optional url)
  (write-file file (get-html (or (current-url) url)))
  #f)
;; copy:
(define (yank)
  "yank string."
  #f)
;; cut:
(define (kill str)
  "kill-new"
  #f)
;; readline-like-shortcuts:
;; ?
;; cancel:
(define* (keyboard-quit #:optional process)
  "Discard process."
  #f)
;; default-search-provider:
(define search-providers
  (circular-list '("search1.com/q?=~a" "ddg.com/q?=~a")))

;; buffers: buffer=webview or buffer =~ tab?
(define-record-type <buffer>
  (make-buffer name)
  buffer?
  (name buffer-name))
(define (kill-buffer buffer)
  "Kill the buffer"
  #f)
(define* (switch-buffer buffer #:optional tab)
  "Switch to buffer in
tab."
  #f)
(define (new-buffer buffer)
  "Create buffer"
  #f)

;; fix:
(define (exit)
  "Fix: save session and exit. BROKEN atm."
  #f)
;; mute:
(define* (mute bool #:optional buffer)
  "If bool=t mute buffer, otherwise unmute it. if no buffer, then
assume current-buffer."
  #f)
;; bookmarks:
(define-record-type <bookmark>
  (make-bookmark name)
  bookmark?
  (name bookmark-name))
;; (define bookmark?
;;   "return t if bookmark."
;;   #f)
(define (new-bookmark)
  "Add new bookmark."
  #f)
(define (remove-bookmark)
  "Remove bookmark."
  #f)
(define (open-bookmark)
  ;; (begin
  ;;   (browse (completing-read "select bookmark: " (bookmarks-list))))
  #f)
;; history:
(define-record-type <history>
  (make-history name)
  history?
  (name history-name))
;; implement history in scheme or use webkit history? history is just
;; another form of bookmarks, a compulsory, runtime bookmarks
;; (define (history?)
;;   #f)
(define (remove-history)
  #f)
(define (add-history)
  #f)
(define (find-history)
  #f)
;; download:
(define (start-download)
  #f)
(define (stop-download)
  #f)
(define (list-downloads)
  #f)
(define (download-directory)
  #f)

;; Search
(define* (string-search str #:optional buffer)
  "Search for a string str in buffer contents."
  ;; search link: (search (string-append "<a href=" (input)) (get-html
  ;; (current-url)))
  ;; search text: (string-search (input) (get-html (current-url)))

  #f)
;; isearch:
(define (isearch)
  "isearch from emacs."
  ;; (string-search (input "isearch: "
  ;;                       (get-html (current-url))))

  #f)

;; Edit url/like URL bar in a browser
(define (edit-url)
  "edit current-url in minibuffer (with completions?)"
  #f)
;; define-key:
(define (set-key map key symbol)
  "like emacs define-key or global-set-key."
  #f)
;; mouse-* events: "Mouse-1" - left click, "Mouse-2" - right-click, "Mouse-3" - middle-click

;; input/ui
;; read-from-minibuffer:
(define (input prompt)
  "input prompt. like emacs read-from-minibuffer"
  #f)
;; completing-read:
(define (completing-read prompt)
  "completing read from emacs."
  ;; (begin (render-popup ...) (input prompt))

  #f)
