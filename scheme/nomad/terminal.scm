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

(define-module (nomad terminal)
  #:use-module (emacsy emacsy)
  #:use-module (nomad text)
  #:use-module (oop goops)
  #:export (<terminal>))

(define shell-map (make-keymap))

(define-public %default-shell "/bin/bash")

(define-public shell-mode (make <mode>
                              #:mode-name "Shell"
                              #:mode-map shell-map))

(define-class <terminal> (<widget-buffer>)
  (name #:init-value "<terminal>"))

(define-interactive (terminal)
  (make <terminal> #:buffer-modes `(,shell-mode)))
