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
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (nomad webview)
  #:use-module (nomad views)
  #:use-module (nomad eval)
  #:use-module (nomad minibuffer)
  #:use-module (nomad window)
  #:use-module (nomad repl)
  #:export (make-buffer-socket
            buffer-pointer
            buffer-uri
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

(define (buffer-uri buffer)
  "Returns the webview URI for BUFFER"
  (with-buffer buffer
    (local-var 'uri)))

(define (buffer-pointer buffer)
  (with-buffer buffer
    (local-var 'web-buffer)))

(define-public (buffers-contains? uri)
  "Returns #t of buffer-list contains URI"
  (let ((contains #f))
    (for-each (lambda buffer
                (when (string= uri
                               (buffer-name (car buffer)))
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

(define (on-webview-enter)
  (when (local-var 'web-buffer)
    (format #t
            "Setting GTK control to ~a~%"
            (local-var 'web-buffer))
    (set-web-buffer! (current-buffer))
    (use-local-map webview-map)))

(define (on-webview-kill)
  (format #t
          "Destroying web-view ~a~%"
          (local-var 'web-buffer))
  (destroy-web-buffer! (local-var 'web-buffer)))

(define-interactive (make-buffer-content #:optional (name (read-from-minibuffer "Name: "))(content (read-from-minibuffer "Content: ")))
  "Creates a new webview buffer with NAME and CONTENT"
  (let ((buffer (switch-to-buffer name)))
    (text-buffer->webview! buffer #f)
    (on-webview-enter)
    (load-content content)))

(define-interactive (make-buffer #:optional (url (read-from-minibuffer "Url: ")))
  "Creates a new webview-bufer with URL"
  (let ((buffer (switch-to-buffer url)))
     (text-buffer->webview! buffer #t)
     (on-webview-enter)
    (set-current-uri! (prefix-url url))))

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

;; FIXME: use a webview-buffer instead of converting text-buffers
(define-public (text-buffer->webview! buffer update)
  "converts in place a text BUFFER to webview buffer"
  (with-buffer buffer
               (set! (local-var 'web-buffer)
                     (make-web-buffer))
               (set! (local-var 'update)
                     update)
               (add-hook! (buffer-enter-hook buffer)
                          on-webview-enter)
               (add-hook! (buffer-kill-hook buffer)
                          on-webview-kill)))

(define-public (update-buffer-names)
  "Updates web buffer names to it's current URI"
  (for-each (lambda (buffer)
              (with-buffer buffer
                           (when (and (webview-buffer? (current-buffer))
                                      (local-var 'update))
                             (set! (local-var 'uri)
                                   (primitive-buffer-uri (local-var 'web-buffer)))
                             (set-buffer-name! (local-var 'uri)))
                           (when (and (or (string= (buffer-name (current-buffer))
                                                   "*Messages*")
                                          (string= (buffer-name (current-buffer))
                                                   "*scratch*"))
                                      (not (webview-buffer? (current-buffer))))
                             (text-buffer->webview! buffer #f)
                             (when (not (notebook-contains buffer))
                               (notebook-insert buffer 0))
                             ;; FIXME: do not hard code HTML use SXML views to display *Messages* and
                             ;; scratch and messages?
                             (load-content (format #f
                                                   "<H2>~a<H2>"
                                                   (buffer-name (current-buffer)))))))
            (buffer-list)))

;; (define-key global-map (kbd "C-b") (lambda _
;;                                      (next-buffer)
;;                                      (when (string= (buffer-name (current-buffer))
;;                                                     "*scratch*")
;;                                        (next-buffer))))

(define-key global-map (kbd "C-b") 'next-buffer)

;; (define-key global-map (kbd "C-x C-b") 'next-buffer)
