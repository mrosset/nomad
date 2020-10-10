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
  #:use-module (ice-9 receive)
  #:use-module (nomad menu)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad widget)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<ibuffer>
            <ibuffer-entry>))

(define ibuffer-map (make-keymap))

(define header-lines 2)
(define hidden-buffer 1)

;; Line offset is 2 header lines + one line for the hidden ibuffer.
(define line-offset (+ header-lines hidden-buffer))

(define ibuffer-mode (make <mode>
                              #:mode-name "IBuffer"
                              #:mode-map ibuffer-map))

(define-class <ibuffer-entry> ()
  (buffer #:accessor !buffer #:init-keyword #:buffer #:init-value #f)
  (key    #:accessor !key    #:init-keyword #:key)
  (marks  #:accessor !marks  #:init-form (make-list 1 #f)))

(define-method (unmark (self <ibuffer-entry>))
  (set! (!marks self) (make-list 1 #f)))

(define-method (mark-entry (self <ibuffer-entry>) mark)
  (list-set! (!marks self) 0 mark))

(define-method (marked? (self <ibuffer-entry>))
  (car (!marks self)))

(define-method (apply-marks (self <ibuffer-entry>))
  (for-each (lambda (mark)
              (case mark
                ((D)
                 (with-buffer (!buffer self)
                   (kill-buffer))
                 (set! (!marks self) (make-list 3 #f)))
                (else #f)))
            (!marks self))
  (set! (!buffers (current-buffer)) '())
  (update))

(define-class <ibuffer> (<widget-buffer> <text-buffer>)
  (buffers     #:accessor  !buffers
               #:init-form '())
  (last-buffer #:accessor  !last-buffer
               #:init-form last-buffer))

(define-method (initialize (buffer <ibuffer>) args)
  (next-method)
  (add-hook! (buffer-enter-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #f)
               (set! (!last-buffer (current-buffer)) last-buffer)
               (update)))
  (add-hook! (buffer-kill-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #t)))
  (add-hook! (buffer-exit-hook buffer)
             (lambda ()
               (%inhibit-menu-bar #t))))

(set! buffer-classes (cons <ibuffer>
                            buffer-classes))

(define-method (marks (self <ibuffer>))
  (filter (lambda (item)
            (marked? (cdr item)))
          (!buffers self)))

(define (insert-header)
  (insert (format #f "MR     Name ~/~/   Uri/Filename~%"))
  (insert (format #f "--     -----~/~/   ------------~%")))

(define (ibuffer-line index entry)
  (let ((buffer (!buffer entry))
        (mark   (list-ref (!marks entry) 0)))
    (format #f "~a ~a ~a      ~a~/~/    ~a\n"
            (or mark "")
            index
            (line-number-at-pos)
            (buffer-name buffer)
            (if (is-a? buffer <web-buffer>)
                (buffer-uri buffer)
                (or (buffer-file-name buffer)
                    "--")))))

(define (insert-buffers buffers)
  (for-each (lambda (item)
              (insert (ibuffer-line (car item) (cdr item))))
            (reverse buffers)))

(define (buffers->alist index)
  "Converts (buffer-list) to a alist starting with @var{index}."
  (let ((lst '()))
    (for-each (lambda (b)
                (unless (eq? (current-buffer) b)
                  (set! lst (acons index (make <ibuffer-entry> #:buffer b) lst))
                  (set! index (1+ index))))
              (reverse (buffer-list)))
    lst))

(define (update-buffer-list)
  "Compares the ibuffers buffer list with emacys (buffer-list). Adding new buffers to the !buffers slot"
  (let ((index (+ (length (!buffers (current-buffer)))
                  line-offset))
        (new (map (lambda (b)
                    (if (not (find (lambda (e)
                                     (eq? b (!buffer (cdr e))))
                                   (!buffers (current-buffer))))
                        b
                        #f))
                  (buffer-list))))
    (for-each (lambda (b)
                (when b
                  (unless (eq? (buffer-name b) "ibuffer")
                    (set! (!buffers (current-buffer))
                          (acons index
                                 (make <ibuffer-entry> #:key index #:buffer b)
                                 (!buffers (current-buffer))))
                    (set! index (1+ index)))))
              (reverse new))))

(define* (update #:optional (line 2))
  (delete-region (point-min) (point-max))
  (insert-header)
  (let ((current (line-number-at-pos)))
    (update-buffer-list)
    (insert-buffers (!buffers (current-buffer)))
    (backward-delete-char 1)
    (goto-char (point-min))
    (ibuffer-forward-line line)))

(define-interactive (ibuffer-forward-line #:optional (n 1))
  "Move forward @var{n} lines. Default is one line."
  (do ((i 0 (1+ i)))
      ((>= i n))
   (when (<= (line-number-at-pos) (count-lines))
    (forward-line))))

(define-interactive (ibuffer-backward-line)
  "Move backward one line"
  (when (>= (line-number-at-pos) 4)
    (backward-line)))

(define-method (entry-at-line)
  (assoc-ref (!buffers (current-buffer)) (line-number-at-pos)))

(define-method (buffer-at-line (ibuffer <ibuffer>))
  (!buffer (entry-at-line)))

(define (buffer-name-at-line)
  (buffer-name (buffer-at-line (current-buffer))))

(define (switch-to)
  (switch-to-buffer (buffer-at-line (current-buffer))))

(define-interactive (ibuffer-mark-for-delete)
  "Mark for deletion the buffer on the current line."
  (unless (entry-at-line)
    (format #t "~a\n" (entry-at-line))
    (error "Cant find entry at line"))
  (mark-entry (entry-at-line) 'D)
  (update (line-number-at-pos)))

(define-interactive (ibuffer #:optional (index 0))
  "Begin using Ibuffer to edit a list of buffers"
  (let* ((current (find (compose (cut is-a? <> <ibuffer>))
                        (buffer-list)))
         (buffer  (if current
                      (switch-to-buffer current)
                      (make-buffer <ibuffer>
                                   #:name         "ibuffer"
                                   #:buffer-modes `(,ibuffer-mode)))))
    #t))

(define-interactive (ibuffer-do-kill-on-deletion-marks)
  "Kill buffers marked for deletion"
  (let* ((marked (marks (current-buffer)))
         (total  (length marked))
         (prompt (format #f "Really kill ~a buffers? (y or n) " total)))
    (when (and (> total 0)
               (string= "y" (read-from-minibuffer prompt)))
      (for-each (lambda (item)
                  (apply-marks (cdr item)))
                marked)
      (update (line-number-at-pos)))))

(define-interactive (ibuffer-unmark-forward)
  "Umarks the current buffer and moves forward."
  (unmark (entry-at-line))
  (update (line-number-at-pos)))

;; Ibuffer map
(define-key ibuffer-map "RET" switch-to)
(define-key ibuffer-map "g" (lambda _ (update (line-number-at-pos))))
(for-each (lambda (key)
            (define-key ibuffer-map key ibuffer-forward-line))
          '("C-n" "n"))
(for-each (lambda (key)
            (define-key ibuffer-map key ibuffer-backward-line))
          '("C-p" "p"))
(define-key ibuffer-map "ESC" kill-buffer)
(define-key ibuffer-map "d" ibuffer-mark-for-delete)
(define-key ibuffer-map "x" ibuffer-do-kill-on-deletion-marks)
(define-key ibuffer-map "u" ibuffer-unmark-forward)
(define-key ibuffer-map "q" kill-buffer)

;; Global map
(define-key global-map (kbd "C-x b") 'ibuffer)
