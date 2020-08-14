;; ibuffer.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad ibuffer)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (nomad text)
  #:use-module (nomad web)
  #:use-module (nomad util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<ibuffer>))

(define ibuffer-map (make-keymap))

(define-public ibuffer-mode (make <mode>
                              #:mode-name "IBuffer"
                              #:mode-map ibuffer-map))

(define-class <ibuffer> (<widget-buffer> <text-buffer>)
  (index   #:accessor   !index
           #:init-value 0)
  (buffers #:accessor   !buffers
           #:init-thunk buffer-list))

(set! buffer-classes (cons <ibuffer>
                            buffer-classes))

(define (ibuffer-header)
  (insert (format #f "MR     Name ~/~/   Uri/Filename~%"))
  (insert (format #f "--     -----~/~/   ------------~%")))

(define (ibuffer-line buffer)
  (format #f "       ~a~/~/    ~a\n"
          (buffer-name buffer)
           (if (is-a? buffer <web-buffer>)
               (buffer-uri buffer)
               (or (buffer-file-name buffer)
                   "--"))))

(define* (update #:optional (buffer (current-buffer)))
  (delete-region (point-min) (point-max))
  (ibuffer-header)
  (set! (!index buffer) 0)
  (set! (!buffers buffer)
        (filter-map
         (lambda (b) (if (is-a? b <ibuffer>)
                         #f
                         b))
         (buffer-list)))
  (for-each (lambda (b)
              (insert (ibuffer-line b)))
            (!buffers buffer))
  (backward-delete-char 1)
  (goto-char (point-min))
  (forward-line 2))

(define* (ibuffer-forward-line #:optional (n 1))
  (let* ((buffer  (current-buffer))
         (index   (!index buffer)))
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (when (< index (- (length (!buffers buffer)) 1))
        (set! (!index buffer) (+ index 1))
        (forward-line)))))

(define (ibuffer-backward-line)
  (let* ((buffer  (current-buffer))
         (index   (!index buffer)))
    (when (> index 0)
      (set! (!index buffer) (- index 1))
      (backward-line))))

(define (switch-to)
  (let ((buffer (current-buffer)))
    (switch-to-buffer
     (list-ref (!buffers buffer) (!index buffer)))))

(define-interactive (ibuffer-kill-buffer)
  (let* ((buffer   (current-buffer))
         (selected (list-ref (!buffers buffer) (!index buffer)))
         (index    (!index buffer)))
    (with-buffer selected
      (kill-buffer))
    (switch-to-buffer buffer)
    (update buffer)
    (ibuffer-forward-line index)))

(define-interactive (ibuffer #:optional (index 0))
  (let* ((current (find (compose (cut is-a? <> <ibuffer>))
                        (buffer-list)))
         (buffer  (if current
                      (switch-to-buffer current)
                      (make-buffer <ibuffer>
                                   #:name         "ibuffer"
                                   #:buffer-modes `(,ibuffer-mode)))))
    (update buffer)
    (ibuffer-forward-line index)))

(define-key ibuffer-map "RET" switch-to)
(define-key ibuffer-map "g" (lambda _ (update (current-buffer))))
(for-each (lambda (key)
            (define-key ibuffer-map key ibuffer-forward-line))
          '("C-n" "n"))
(for-each (lambda (key)
            (define-key ibuffer-map key ibuffer-backward-line))
          '("C-p" "p"))
(define-key ibuffer-map "ESC" kill-buffer)
(define-key ibuffer-map "d" ibuffer-kill-buffer)
(define-key ibuffer-map "q" kill-buffer)
