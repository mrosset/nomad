;; Copyright (C) 2019  Amar Singh

;; Nomad --- An extensible web browser

;; This file is part of Nomad.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


(define-module (nomad bookmark))
(use-modules
 (nomad eval)
 (nomad browser)
                                        ;; (nomad buffer)
 )

(define bookmarks '(("guilem" . "https://www.gnu.org/software/guile/manual/html_node")
		    ("emacs" . "https://www.gnu.org/software/emacs")
		    ("guile" . "https://www.gnu.org/software/guile")
		    ("google" . "https://www.google.ca")
		    ("etoscheme" . "https://www.cs.utexas.edu/~novak/schemevscl.html")))

(define (pp-bookmarks)
  (define (print-bookmark arg)
    (format #t "~s\t~s\n" (car arg) (cdr arg)))
  (map print-bookmark bookmarks))

(define-command (open-book key)
  "Opens bookmark by key in current buffer"
    (browse (book key)))

(define (book key)
  (assoc-ref bookmarks key))

(define-command (make-book key)
  "Opens bookmark by 'KEY in a new buffer"
  (make-buffer (book key)))
