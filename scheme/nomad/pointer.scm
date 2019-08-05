;; pointer.scm
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

(define-module (nomad pointer)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:use-module (nomad frame)
  #:use-module (g-golf)
  #:use-module (nomad util)
  #:use-module (system foreign)
  )

;; (gi-import "Gtk")

(define-class-public <widget-buffer>
  (<text-buffer>)
  (widget #:accessor !widget #:init-keyword #:widget)
  (pointer #:accessor !pointer #:init-keyword #:pointer #:init-value %null-pointer))

(define-method (get-pointer (buffer <widget-buffer>))
  (let* ((widget (slot-ref buffer 'widget))
         (pointer (slot-ref widget 'g-inst)))
    pointer))

(define-method-public (buffer-pointer (buffer <widget-buffer>))
  (get-pointer buffer))

(define-method-public (buffer-pointer)
  (buffer-pointer (current-buffer)))

;; (define-method-public (set-buffer-pointer! pointer)
;;   (set-buffer-pointer! (current-buffer)
;;                        pointer))

(define-public (pointer-kill-hook)
  (info "Destroying pointer ~a"
        (buffer-pointer))
  (destroy-pointer (buffer-pointer (current-buffer))))

(define-public (pointer-enter-hook)
  (info "Setting pointer to ~a"
        (buffer-pointer))
  (switch-to-pointer (buffer-pointer (current-buffer)))
  (gtk-widget-grab-focus (slot-ref (current-buffer)
                                    'widget)))
