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
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (make-frame-socket
            <nomad-frame>))

(eval-when (expand load eval)
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("Gtk" . "Widget")
         ("Gtk" . "Label")
         ("Gio" . "Application")
         ("Gtk" . "Notebook"))))



(define-class <nomad-frame> ())



(define-public (current-frame)
  "Returns the current frame"
  (let* ((app (g-application-get-default))
         (frame (gtk-application-get-active-window app)))
    frame))

;; (define (make-frame-socket url socket)
;;   "Write `make-frame' comand with arg URL to a SOCKET."
;;   (write-socket (format #f "~S" `(make-frame ,url)) socket))
