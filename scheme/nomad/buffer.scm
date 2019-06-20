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
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (nomad app)
  #:use-module (nomad repl)
  #:use-module (nomad minibuffer)
  #:use-module (nomad views)
  #:use-module (nomad eval))

(define-public (buffer-with-id key)
  "Returns a buffer from the buffer alist with ID. If a buffer with ID
is not found returns #f"
  (let ((pair (assv key (buffer-alist))))
    (if pair (cdr pair)
        #f)))

(define-public (make-buffer-socket url socket)
  "Write `make-buffer' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-buffer ,url))
                   socket))

(define-command (kill-some-buffers)
  "Kill all buffers but one"
  ;; doings something weird, not using the argument?
  (for-each (lambda (arg)
              (kill-buffer)) (buffer-alist)))

(define-public (buffers->list)
  "Returns a list of uri's for all buffers"
  (map (compose buffer-uri cdr)
       (buffer-alist)))

(define (format-buffer buffer)
  "Returns a human readable buffer string in 80 column format"
  (format #f "id: ~80:@y\t uri: ~80:@y"
          (car buffer)
          (buffer-uri (cdr buffer))))

(define-command (list-buffers)
  "Displays buffers in minipopup"
  (begin
    (render-popup completion-view (buffers->list) -1)
    (length (buffers->list))))

;; (define-command (buffers)
;;   "Returns a string of all buffers pretty printed"
;;   (with-output-to-string (lambda _
;;                            (format #t "~a" (pretty-print (buffers->list))))))

(define-command (pp-buffers)
  "Pretty prints buffers-alist."
  (with-output-to-string
    (lambda _
      (for-each (compose (cut format #t "~a\n" <>)
                         format-buffer)
                (buffer-alist)))))
