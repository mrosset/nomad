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
  #:use-module (nomad eval)
  #:use-module (emacsy emacsy)
  #:use-module (nomad init)
  #:export (current-selection
	    current-view
	    current-list))

(define current-selection 0)
(define current-view #nil)
(define current-list '())

(define-public minibuffer-mode-map '(("C-n" . (next-line))
				     ("C-p" . (previous-line))))

(define-public (current-selection-ref)
  (let* ((lst current-list)
	 (item (list-ref lst current-selection)))
    (string->symbol item)))

(define-public (reset-minibuffer)
  (set! current-view #nil)
  (set! current-list '())
  (set! current-selection 0)
  (hide-minibuffer-popup))

(define-interactive (minibuffer-execute)
  (let* ((ref (local-var 'selection))
	 (sym (list-ref (commands global-cmdset)
			ref))
	 (proc (eval sym
		     (interaction-environment))))
    ;; (command-execute proc)
    (exit-minibuffer)))

(define-interactive (next-line)
  (let ((row (+ (local-var 'selection) 1))
	(view (local-var 'view))
	(lst (local-var 'list)))
    (when (not (>= row (length lst)))
      (set! (local-var 'selection)
	    row)
      (render-popup view lst row))))

(define-interactive (previous-line)
  (let ((row (- (local-var 'selection) 1))
	(view (local-var 'view))
	(lst (local-var 'list)))
    (when (not (= row 0))
      (set! (local-var 'selection) row)
      (render-popup view lst row))))

(define-public (input-completion text)
  "Returns a list of command symbols matching 'TEXT"
  (let ((completion '()))
    (map (lambda (p)
	   (let ((key (symbol->string (car p))))
	     (when (string-match text key)
	       (set! completion (append completion (list key))))))
	 command-alist)
    completion))

(define-public (history-completion text)
  "Returns a list of matches in history list"
  (let ((completion '()))
    (map (lambda (s)
	   (when (string-match text s)
	     (set! completion (append completion (list s)))))
	 history)
    completion))
