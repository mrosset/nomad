;; frame.scm
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

(define-module (nomad frame)
  #:use-module (emacsy emacsy)
  #:use-module (nomad lib)
  #:use-module (g-golf)
  #:export (make-frame-socket))

(load-extension (dynamic-path) "init_guile_nomad_frame")

(gi-import "Nomad")

(define-public (notebook-insert buffer pos)
  (let ((widget (slot-ref buffer 'widget)))
    (nomad-app-frame-notebook-insert widget
                                     (buffer-name buffer)
                                     pos)
    (nomad-widget-show-all widget)))

(define (make-frame-socket url socket)
  "Write `make-frame' comand with arg URL to a SOCKET."
  (write-socket (format #f "~S" `(make-frame ,url)) socket))
