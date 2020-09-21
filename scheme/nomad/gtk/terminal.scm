;; terminal.scm
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

(define-module (nomad gtk terminal)
  #:use-module (ice-9 format)
  #:use-module (nomad gtk gi)
  #:use-module (nomad widget)
  #:use-module (nomad terminal)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

(set-current-module (resolve-module '(nomad terminal)))

(use-modules (g-golf))

(define-method (emacsy-mode-line (buffer <terminal>))
  (let ((vte (buffer-widget buffer)))
    (format #f "~a~/~a"
            (next-method)
            (get-window-title vte))))
