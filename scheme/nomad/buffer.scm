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
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (make-buffer-socket
            buffers-contain?
            buffers->uri))

(define (make-buffer-socket url socket)
  "Write `make-buffer' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-buffer ,url))
                   socket))

(define-interactive (kill-some-buffers)
  "Kill all buffers but the message buffer"
  (for-each (lambda (buffer)
              (switch-to-buffer buffer)
              (kill-buffer))
            (buffer-list)))

(define (buffers-contain? uri)
  "Returns #t if buffer-list contains @var{uri}"
  (if (is-a? (find (lambda (buffer)
               (and (is-a? buffer <web-buffer>)
                    (string= (widget-uri buffer) uri)))
                   (buffer-list))
             <web-buffer>)
      #t
      #f))

(define (buffers->uri)
  "Returns a list of uri's for all buffers"
  (filter-map (lambda (buffer)
                (if (is-a? buffer <web-buffer>)
                    (widget-uri buffer)
                    #f))
              (buffer-list)))

(define-interactive (message-buffers)
  "Pretty prints the buffers to echo area"
  (message "~a" (with-output-to-string (lambda _ (pretty-print (buffer-list))))))

(define-interactive (make-content-buffer #:optional (name (read-from-minibuffer "Name: "))
                                         (content (read-from-minibuffer "Content: ")))
  "Creates a new webview buffer with NAME and CONTENT"
  (let ((buffer (make-webcontent-buffer name content)))
    (with-buffer buffer
                 (set-buffer-hooks!)
                 (buffer-render))
    (switch-to-buffer buffer)
    buffer))

(define-public (switch-if-not-current buffer)
  "Switch to buffer if it's not the current buffer already. Returns #t if buffer switched"
  (if (eq? buffer (current-buffer))
      #f
      (begin (switch-to-buffer buffer)
             #t)))

(define-interactive (rename-buffer
                     #:optional
                     (input (read-from-minibuffer "Name: "))
                     (buffer (current-buffer)))
  "Renames the @var{buffer} to @var{input}."
  (set-buffer-name! input buffer))

(define-interactive (eval-buffer #:optional (buffer (current-buffer)))
  (catch #t
    (lambda _
      (let ((val (eval-string (buffer:buffer-string buffer))))
        (pretty-message "~a" val)
        val))
    (lambda (key . vals)
      (message "Error: key: ~a value: ~a" key vals))))

(define-interactive (class-of-buffer #:optional (buffer (current-buffer)))
  (message "~a" (class-of buffer))
  #t)

(define-key fundamental-map (kbd "C-c C-b") eval-buffer)
