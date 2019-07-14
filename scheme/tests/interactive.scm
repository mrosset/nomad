;; interactive --- interactive tests for Nomad

;; Copyright (C) 2019 Amar Singh<nly@disroot.org>

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

;;; Commentary:
;;; this script runs interactive tests in a nomad instance.

(define-module (tests interactive))

;;; Usage:
;;; from a shell:
;;; $ cd nomad-source
;;; $ guile scheme/tests/interactive.scm
;;; ... then check graphical.log file

;;; nomad/ source dir
(define dir (dirname
             (dirname (dirname (current-filename)))))

;;; helpers
(define (src/ str) (string-append dir "/" str))

;;; use latest source
(add-to-load-path "scheme/")
(load (src/ "scheme/nomad/repl.scm"))
(use-modules (nomad repl))

(define (cat . args) (string-join args " "))

;;; socket file
(define soc "/tmp/nomad-socket")

;;; call nomad(built from source) in a guix environment
(system
 (cat "guix environment -l" (src/ "guix-local.scm")
      "&&"
      (src/ "pre-inst-env") (src/ "src/nomad")
      "&"))
;;; wait for socket to be initialized
(sleep 7)

;;; load graphical tests
(write-socket "(use-modules (tests graphical))\n" soc)
(sleep 7)

;;; exit nomad
;;; FIXME: if not using sleep after write-socket, doesn't kill nomad
(write-socket "(kill-nomad)" soc)
(sleep 7)
