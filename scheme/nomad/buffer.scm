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
  #:use-module (nomad eval)
  #:use-module (nomad minibuffer)
  #:use-module (nomad repl)
  #:use-module (nomad views)
  #:use-module (nomad webkit)
  #:use-module (nomad webview)
  #:use-module (nomad frame)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (make-buffer-socket
            update-buffers
            buffers-contain?
            buffer-protected?
            buffers->uri))

(define protected-buffers '("*scratch*" "*Messages*"))

(define (buffer-protected? buffer)
  "Returns true if buffer is protected and should not be deleted."
  (find (lambda (item)
          (equal? item (buffer-name buffer)))
        protected-buffers))

(define (make-buffer-socket url socket)
  "Write `make-buffer' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-buffer ,url))
                   socket))

(define-interactive (kill-some-buffers)
  "Kill all buffers but the message buffer"
  (for-each (lambda (buffer)
              (unless (buffer-protected? buffer)
                (switch-to-buffer buffer)
                (kill-buffer)))
            (buffer-list)))

(define (buffers-contain? uri)
  "Returns #t of buffer-list contains URI"
  (let ((contains #f))
    (for-each (lambda (buffer)
                (when (string= uri
                               (buffer-name buffer))
                  (set! contains #t)))
              (buffer-list))
    contains))

(define (buffers->uri)
  "Returns a list of uri's for all buffers"
  (map (lambda (buffer)
         (with-buffer buffer
           (let ((uri (buffer-name buffer)))
             (when (and uri
                        (not (string= uri "nomad://")))
               uri))))
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

(define-interactive (make-content-buffer #:optional (name (read-from-minibuffer "Name: "))
                                         (content (read-from-minibuffer "Content: ")))
  "Creates a new webview buffer with NAME and CONTENT"
  (let ((buffer (make-webcontent-buffer name content)))
    (with-buffer buffer
                 (set-buffer-pointer! (webkit-new))
                 (set-buffer-hooks!)
                 (buffer-render))
    (switch-to-buffer buffer)
    buffer))

(define-interactive (make-buffer #:optional (url (read-from-minibuffer "Url: ")))
  "Creates a new webview-bufer with URL"
  (let ((buffer (make-webview-buffer url)))
    (with-buffer buffer
                 (set-buffer-pointer! (webkit-new))
                 (set-buffer-hooks!)
                 (set-buffer-uri! (prefix-url url)))
    (switch-to-buffer buffer)
    buffer))

(define (webview-buffer? buffer)
  (let ((iswebview #f))
    (with-buffer buffer
      (catch 'no-such-local-variable
        (lambda _
          (when (local-var 'web-buffer)
            (set! iswebview #t)))
        (lambda (key . param)
          (set! iswebview #f))))
    iswebview))

;; FIXME: This should probably not be needed. But if certain buffers are
;; killed then they are no longer webviews. And so can not be switched to or
;; managed properly.
(define (update-buffers)
  "Converts buffers to <webview-buffer> and inserts them into notebook"
  (for-each (lambda (buffer)
              (unless (eq? <webview-buffer> (class-of buffer))
                (buffer->webview-buffer buffer)
                (set-buffer-pointer! buffer (webkit-new))
                (buffer-render buffer)
                (notebook-insert buffer 0)))
            (buffer-list)))
