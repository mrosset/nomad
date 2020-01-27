;; gtk.scm --- Nomad union module

;; Copyright (C) 2019  Amar Singh<nly@disroot.org>
;; Copyright (C) 2019-2020  Mike Rosset@gmail.com

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

(define-module (nomad gtk gtk)
  #:use-module (nomad util)
  #:use-module (oop goops)
  #:use-module (nomad gtk application)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk graph)
  #:export (<platform-webview-buffer>
            <platform-application>
            <platform-frame>))

(define-class <platform-webview-buffer> (<gtk-webview-buffer>))

(define-class <platform-application> (<nomad-gtk-application>))

(define-class <platform-frame> (<gtk-frame>))

(re-export-modules
 '(nomad gtk application)
 '(nomad gtk buffers)
 '(nomad gtk frame)
 '(nomad gtk graph))
