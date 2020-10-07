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
  #:use-module (nomad widget)
  #:use-module (nomad web)
  #:use-module (nomad util)
  #:use-module (nomad menu)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<ibuffer>
            !buffers))

(define ibuffer-map (make-keymap))

(define header-lines 2)
(define hidden-buffer 1 )

;; Line offset is 2 header lines + one line for the hidden ibuffer.
(define line-offset (+ header-lines hidden-buffer))

(define-public ibuffer-mode (make <mode>
                              #:mode-name "IBuffer"
                              #:mode-map ibuffer-map))

(define-class <ibuffer> (<widget-buffer> <text-buffer>)
  (buffers #:accessor !buffers #:init-value #f))

(define-method (initialize (buffer <ibuffer>) args)
  (next-method)
  (add-hook! (buffer-enter-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #f)
               (update buffer)))
  (add-hook! (buffer-kill-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #t)))
  (add-hook! (buffer-exit-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #t))))

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
  (set! (!buffers buffer)
        (filter-map
         (lambda (b) (if (is-a? b <ibuffer>)
                         #f
                         b))
         (reverse (buffer-list))))
  (for-each (lambda (b)
              (insert (ibuffer-line b)))
            (!buffers buffer))
  (backward-delete-char 1)
  (goto-char (point-min))
  (forward-line header-lines))

(define-interactive (ibuffer-forward-line #:optional (n 1))
  (do ((i 0 (1+ i)))
      ((>= i n))
   (when (<= (line-number-at-pos) (count-lines))
    (forward-line))))

(define-interactive (ibuffer-backward-line)
  (when (>= (line-number-at-pos) 4)
    (backward-line)))

(define (buffer-at-line ibuffer)
  (list-ref (!buffers ibuffer) (- (line-number-at-pos) line-offset)))

(define (switch-to)
  (switch-to-buffer
   (buffer-at-line (current-buffer))))

(define-interactive (ibuffer-kill-buffer)
  (let* ((buffer   (current-buffer))
         (selected (buffer-at-line buffer))
         (line     (- (line-number-at-pos) line-offset)))
    (with-buffer selected
      (kill-buffer))
    (switch-to-buffer buffer)
    (ibuffer-forward-line line)))

(define-interactive (ibuffer #:optional (index 0))
  (let* ((current (find (compose (cut is-a? <> <ibuffer>))
                        (buffer-list)))
         (buffer  (if current
                      (switch-to-buffer current)
                      (make-buffer <ibuffer>
                                   #:name         "ibuffer"
                                   #:buffer-modes `(,ibuffer-mode)))))
    (ibuffer-forward-line index)))

;; Ibuffer map
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

;; Global map
(define-key global-map (kbd "C-x b") 'ibuffer)
