;; help-mode.scm
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

(define-module (nomad help-mode)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:export (<help-buffer>
            help-mode))

(define help-mode (make <mode> #:mode-name "Help"))

(define-class <help-buffer> (<web-buffer>)
  (name #:init-value "*Help*")
  (buffer-modes #:accessor buffer-modes #:init-value `(,help-mode ,web-mode))
  (view #:accessor !view #:init-value #f))


(define-interactive (help)
  (make-buffer <help-buffer>))
