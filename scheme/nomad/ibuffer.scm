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
  #:use-module (g-golf)
  #:use-module (ice-9 format)
  #:use-module (nomad text)
  #:use-module (nomad web)
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

(define (ibuffer-header)
  (insert (format #f "Name~/Uri/Filename\n"))
  (insert (format #f "--------~/--------\n")))

(define (ibuffer-line buffer)
  (format #f "~a~/~/~a\n"
          (buffer-name buffer)
           (if (is-a? buffer <web-buffer>)
               (buffer-uri buffer)
               (or (buffer-file-name buffer)
                   "--"))))

(set! buffer-classes (cons <ibuffer>
                            buffer-classes))

(define (update buffer)
  (delete-region (point-min) (point-max))
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
  (goto-char (point-min)))

(define (ibuffer-forward-line)
  (let* ((buffer  (current-buffer))
         (index   (!index buffer)))
    (when (< index (- (length (!buffers buffer)) 1))
      (set! (!index buffer) (dimfi (+ index 1)))))
  (forward-line))

(define (ibuffer-backward-line)
  (let* ((buffer  (current-buffer))
         (index   (!index buffer)))
    (when (> index 1)
      (set! (!index buffer) (dimfi (- index 1))))
    (backward-line)))

(define (switch-to)
  (let ((buffer (current-buffer)))
    (switch-to-buffer
     (list-ref (!buffers buffer) (!index buffer)))))

(define-interactive (ibuffer)
  (let* ((current (find (compose (cut is-a? <> <ibuffer>))
                        (buffer-list)))
         (buffer  (if current
                      (switch-to-buffer current)
                      (make <ibuffer>
                        #:name         "<ibuffer>"
                        #:buffer-modes `(,ibuffer-mode)
                        #:keymap        global-map))))
    (update buffer)))

(define-key ibuffer-map "RET" switch-to)
(define-key ibuffer-map "g" (lambda _ (update (current-buffer))))
(define-key ibuffer-map "C-n" ibuffer-forward-line)
(define-key ibuffer-map "n" ibuffer-forward-line)
(define-key ibuffer-map "p" ibuffer-backward-line)
(define-key ibuffer-map "C-p" ibuffer-backward-line)
(define-key ibuffer-map "ESC" kill-buffer)
(define-key ibuffer-map "q" kill-buffer)
