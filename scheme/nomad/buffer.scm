;; buffer.scm
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

(define-module (nomad buffer)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy buffer)
  #:use-module (emacsy mru-stack) ;; until switch-to-buffer is upstreamed
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (nomad webview)
  #:use-module (nomad views)
  #:use-module (nomad eval)
  #:use-module (nomad minibuffer)
  #:use-module (nomad window)
  #:use-module (nomad repl)
  #:export (make-buffer-socket
            buffers->uri))

(define (make-buffer-socket url socket)
  "Write `make-buffer' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-buffer ,url))
                   socket))

(define-interactive (kill-some-buffers)
  "Kill all buffers but the message buffer"
  (for-each (lambda (buffer)
              (kill-buffer buffer))
            (buffer-list)))

(define (buffer-uri buffer)
  "Returns the webview URI for 'buffer"
  (with-buffer buffer
    (primitive-buffer-uri (local-var 'web-buffer))))

(define (buffers->uri)
  "Returns a list of uri's for all buffers"
  (map (lambda (buffer)
         (buffer-uri buffer))
       (buffer-list)))

(define-interactive (show-buffers)
  "Displays buffers in minipopup"
  (begin (render-popup completion-view
                       (buffers->uri)
                       -1)
         (length (buffers->uri))))

(define-interactive (message-buffers)
  "Pretty prints the buffers to echo area"
  (message "~a" (with-output-to-string (lambda _ (pretty-print (buffer-list))))))

(define emacsy-kill-buffer kill-buffer)

(define-interactive (nomad-kill-buffer #:optional (buffer (current-buffer)))
  (when (not (string= "*Messages*"
                      (buffer-name (current-buffer))))
    (with-buffer buffer
      (destroy-web-buffer! (local-var 'web-buffer)))
    (emacsy-kill-buffer buffer)))

;; (set! kill-buffer nomad-kill-buffer)

(define-interactive (make-buffer #:optional (url (read-from-minibuffer "Url: ")))
  (define (on-enter)
    (when (local-var 'web-buffer)
      (format #t
              "Setting web-view to ~a~%"
              (local-var 'web-buffer))
      (set-web-buffer! (local-var 'web-buffer))
      (use-local-map webview-map)))
  (define (on-kill)
    (format #t
            "Destroying web-view ~a~%"
            (local-var 'web-buffer))
    (destroy-web-buffer! (local-var 'web-buffer)))
  (let ((buffer (switch-to-buffer url)))
    (set! (local-var 'web-buffer)
          (make-web-buffer (prefix-url url)))
    (set! (local-var 'update)
          #t)
    (add-hook! (buffer-enter-hook buffer)
               on-enter)
    (add-hook! (buffer-kill-hook buffer)
               on-kill)
    (on-enter)))

(define (webview-buffer? buffer)
  (let ((webview #f))
    (with-buffer buffer
      (catch 'no-such-local-variable
        (lambda _
          (when (local-var 'web-buffer)
            (set! webview #t)))
        (lambda (key . param)
          (set! webview #f))))
    webview))

(define-public (update-buffer-names)
  "Updates web bufffer names to it's current URI"
  (for-each (lambda buffer
              (with-buffer (car buffer)
                (when (and (webview-buffer? (current-buffer))
                           (local-var 'update))
                  (set-buffer-name! (buffer-uri (current-buffer))))))
            (buffer-list)))

;; Skip over Message buffer for now
(define-key global-map (kbd "C-b") 'next-buffer)

;; Prev buffer is not that useful as of now
;; (define-key global-map (kbd "C-n") 'prev-buffer)

(define-key global-map (kbd "C-x C-b") 'message-buffers)
