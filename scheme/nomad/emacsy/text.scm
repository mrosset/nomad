;; text.scm
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

(define-module (nomad emacsy text)
  #:use-module (emacsy emacsy))

;; Switch to the (emacsy emacsy) module and export definitions.
;;
;; TODO: when moving these to emacsy define them in '(emacsy text) instead.
(set-current-module (resolve-module '(emacsy emacsy)))
(export count-lines line-number-at-pos)

(define newline-regex (make-regexp "\\\n"))

(define (count-lines)
  "Returns the number of lines for buffer."
  (let ((done 0))
    (save-excursion
        (goto-char (point-min))
      (while (re-search-forward newline-regex #f #t)
        (set! done (1+ done))))
    done))

(define (line-number-at-pos)
  "Return the buffer line number at current point."
  (let ((opoint (point))
        (line 0))
    (save-excursion
        (goto-char (point-min))
        (while (let ((eol (re-search-forward newline-regex #f #t)))
                 (and eol (>= opoint eol)))
          (set! line (1+ line))))
    (1+ line)))
