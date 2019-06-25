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
  #:use-module (emacsy mru-stack) ;; until switch-to-buffer is upstreamed
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

(define (buffer-uri buffer)
  "Returns the webview URI for 'buffer"
  (with-buffer buffer
               (if (not (string= "*Messages*"
                                   (buffer-name)))
                   (primitive-buffer-uri (local-var 'web-buffer))
                   #f)))

(define (buffers->uri)
  "Returns a list of uri's for all buffers"
  (map (lambda (buffer)
         (buffer-uri buffer))
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
    ;; Create an agenda that updates the buffer name from buffer
    ;; URI. When the buffer URI changes
    (agenda-schedule-interval (lambda ()
                                (with-buffer buffer
                                             (when (not (string= (buffer-name (current-buffer))
                                                                 (buffer-uri buffer)))
                                               (set-buffer-name! (buffer-uri buffer)))))
                              4000)
    (on-enter)))


(define* (primitive-switch-to-buffer buffer #:optional recall?)
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer))
  (run-hook (buffer-exit-hook (current-buffer)))
  (set! last-buffer (current-buffer))
  (if (mru-contains? buffer-stack buffer)
      (begin
        (emacsy-log-debug "Recall buffer ~a" buffer)
        ;; if we want to set the buffer as most recenly used, then
        ;; call with additional non false argument.
        (if recall? (mru-recall! buffer-stack buffer))
        (set! aux-buffer #f))
      (begin
        (emacsy-log-debug "Set buffer to ~a" buffer)
        (set-buffer! buffer)))
  (emacsy-log-debug "Running enter hook for ~a" (current-buffer))
  (run-hook (buffer-enter-hook (current-buffer)))
  (current-buffer))

(define switch-to-buffer* primitive-switch-to-buffer)

(define-interactive (next-buffer* #:optional incr)
  (mru-next! buffer-stack)
  (let ((mru-recall! (lambda (a b) #t)))
    ;; this won't work because this is not elisp, i.e. we have lexical
    ;; scope and closures.
    (switch-to-buffer* (current-buffer))))

(define-key global-map (kbd "C-x C-b") 'message-buffers)
(define-key global-map (kbd "C-b") 'next-buffer*)
(define-key global-map (kbd "C-n") 'prev-buffer)
