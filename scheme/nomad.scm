;; Nomad --- Extensible web browser

;; Copyright (C) 2019  Amar Singh

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

;;; Commentary: support Emacs-style command line flags. Flags processed in scheme.

(define-module (nomad)
  #:use-module (ice-9 getopt-long))

(define scm (current-filename))

(define src (dirname (dirname scm)))

(define (src/ str) (string-append src "/" str))

(system (string-join (list (src/ "pre-inst-env") (src/ "src/nomad") "-c '(boot-nomad)'")
                     " "))

;;; test
(display (command-line))
(newline)
