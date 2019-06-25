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
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (nomad browser)
  #:use-module (nomad views)
  #:use-module (nomad eval)
  #:use-module (nomad minibuffer)
  #:use-module (nomad window)
  #:export (make-buffer-socket
            buffers->uri))

(define (make-buffer-socket url socket)
  "Write `make-buffer' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-buffer ,url))
                   socket))

(define-interactive (kill-some-buffers)
  "Kill all buffers but the message buffer"
  (for-each (lambda _
              (when (not (string= "*Messages*"
                                  (buffer-name (current-buffer))))
                (kill-buffer)))
            (buffer-list)))

(define (buffers->uri)
  "Returns a list of uri's for all buffers"
  (map (lambda (buffer)
         (buffer-name buffer))
       (buffer-list)))

(define (show-buffers)
  "Displays buffers in minipopup"
  (begin
    (render-popup completion-view (buffers->uri) -1)
    (length (buffers->uri))))

(define-interactive (message-buffers)
  "Pretty prints the buffers to echo area"
  (message "~a" (with-output-to-string (lambda _ (pretty-print (buffer-list))))))

(define-interactive (make-buffer #:optional (url (read-from-minibuffer "Url: ")))
  (define (on-enter)
    (when (local-var 'web-buffer)
      (format #t
              "Setting web-view to ~a~%"
              (local-var 'web-buffer))
      (set-web-buffer! (local-var 'web-buffer))))
  (let ((buffer (switch-to-buffer url)))
    (set! (local-var 'web-buffer)
          (make-web-buffer (prefix-url url)))
    (add-hook! (buffer-enter-hook buffer)
               on-enter)
    (on-enter)))

(define mru-next! (@@ (emacsy mru-stack) mru-next!))
(define mru-prev! (@@ (emacsy mru-stack) mru-prev!))
(define mru-ref (@@ (emacsy mru-stack) mru-ref))

(define* (buffer-previous! #:optional (incr 1))
  (mru-prev! buffer-stack incr)
  (switch-to-buffer (mru-ref buffer-stack)))

(define* (buffer-next! #:optional (incr 1))
  (mru-next! buffer-stack (- incr))
  (switch-to-buffer (mru-ref buffer-stack)))

(define-key global-map (kbd "C-x C-b") 'message-buffers)
(define-key global-map (kbd "C-b") 'buffer-next!)
(define-key global-map (kbd "C-n") 'buffer-previous!)
