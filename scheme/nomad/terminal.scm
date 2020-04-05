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
  #:export (<terminal>
            %default-shell
            terminal-foreground
            terminal-background
            terminal-palette))

(define shell-map (make-keymap))

(define-public %default-shell "/bin/bash")

(define terminal-foreground (make-parameter "#ABB2BF"))
(define terminal-background (make-parameter "#282C34"))

(define tango '("rgb(0,0,0)"
                "rgb(204,0,0)"
                "rgb(78,154,6)"
                "rgb(196,160,0)"
                "rgb(52,101,164)"
                "rgb(117,80,123)"
                "rgb(6,152,154)"
                "rgb(211,215,207)"
                "rgb(85,87,83)"
                "rgb(239,41,41)"
                "rgb(138,226,52)"
                "rgb(252,233,79)"
                "rgb(114,159,207)"
                "rgb(173,127,168)"
                "rgb(52,226,226)"
                "rgb(238,238,236)"))

(define terminal-palette tango)

(define shell-mode (make <mode>
                              #:mode-name "Shell"
                              #:mode-map shell-map))

(define-class <terminal> (<widget-buffer>)
  (name #:init-value "*terminal*"))

(set! buffer-classes (cons* <terminal>
                            buffer-classes))

(define-interactive (terminal)
  (make <terminal> #:buffer-modes `(,shell-mode)))
