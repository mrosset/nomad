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
  #:use-module (nomad init)
  #:export (minibuffer-mode-map
	    current-line))

(define current-line 0)

(define minibuffer-mode-map '(("C-n" . (next-line))
			      ("C-p" . (prevous-line))))

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
