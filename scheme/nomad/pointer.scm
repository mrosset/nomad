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
  #:use-module (nomad frame)
  #:use-module (nomad util)
  #:use-module (oop goops)
  #:use-module (system foreign)
  )

(define-class-public <pointer-buffer> (<text-buffer>)
  (pointer #:getter buffer-pointer #:setter set-buffer-pointer! #:init-keyword #:pointer #:init-value %null-pointer))

(define-method-public (buffer-pointer)
  (buffer-pointer (current-buffer)))

(define-method-public (set-buffer-pointer! pointer)
  (set-buffer-pointer! (current-buffer)
                       pointer))

(define-public (pointer-kill-hook)
  (info "Destroying pointer ~a"
        (buffer-pointer))
  (destroy-pointer (buffer-pointer (current-buffer))))

(define-public (pointer-enter-hook)
  (info "Setting pointer to ~a"
        (buffer-pointer))
  (switch-to-pointer (buffer-pointer (current-buffer))))
