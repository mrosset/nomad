;; minibuffer.scm
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

(define-module (nomad minibuffer)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (oop goops)
  #:use-module (nomad views)
  #:use-module (emacsy emacsy)
  #:use-module (nomad init)
  #:export (current-selection
            current-view
            current-list))

(define current-selection 0)
(define current-view #nil)
(define current-list '())

(define (current-selection-ref)
  (let* ((lst current-list)
         (item (list-ref lst current-selection)))
    (string->symbol item)))

(define (reset-minibuffer)
  (set! current-view #nil)
  (set! current-list '())
  (set! current-selection 0)
  (hide-minibuffer-popup))

(define emacsy-minibuffer-complete #f)

;; (set! emacsy-minibuffer-complete minibuffer-complete)

(define (nomad-minibuffer-complete)
  (emacsy-minibuffer-complete)
  (render-completion-popup-view))

;; (set! minibuffer-complete nomad-minibuffer-complete)

(define (minibuffer-execute)
  (let* ((ref (local-var 'selection))
         (sym (list-ref (local-var 'completions)
                        ref))
         (proc (eval (string->symbol sym)
                     (interaction-environment))))
    (if (nomad:command? proc)
        (begin (format #t "COMMAND: ~a\n" sym)
               ;; (command-execute proc)
               (delete-minibuffer-contents minibuffer)
               (with-buffer minibuffer
                 (insert sym))
               (exit-minibuffer))
        (begin (format #t
                       "NOT COMMAND: ~a\n"
                       (class-of sym))
               (exit-minibuffer)))))

;; (define-interactive (next-line)
;;   (let ((row (+ (local-var 'selection) 1))
;;      (view (local-var 'view))
;;      (lst (local-var 'completions)))
;;     (when (not (>= row (length lst)))
;;       (set! (local-var 'selection)
;;          row))
;;     (render-completion-popup-view)))

;; (define-interactive (previous-line)
;;   (let ((row (- (local-var 'selection) 1))
;;      (view (local-var 'view))
;;      (lst (local-var 'completions)))
;;     (when (not (< row 0))
;;       (set! (local-var 'selection) row)
;;       (render-completion-popup-view))))
