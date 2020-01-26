;; nomad.scm --- Nomad union module

;; Copyright (C) 2019  Amar Singh<nly@disroot.org>

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

(define-module (nomad nomad)
  #:use-module (nomad platform)
  #:use-module (nomad application)
  #:use-module (nomad bookmark)
  #:use-module (nomad buffer)
  ;; #:use-module (nomad curl)
  #:use-module (nomad doc)
  #:use-module (nomad download)
  #:use-module (nomad commands)

  #:use-module (nomad frame)
  #:use-module (nomad html)
  #:use-module (nomad init)
  #:use-module (nomad lib)
  #:use-module (nomad minibuffer)
  #:use-module (nomad options)
  #:use-module (nomad widget)
  #:use-module (nomad repl)
  #:use-module (nomad server)
  #:use-module (nomad shroud)
  #:use-module (nomad tests)
  #:use-module (nomad text)
  #:use-module (nomad util)
  #:use-module (nomad views)
  #:use-module (nomad webview))

(re-export-modules
 '(nomad platform)
 '(nomad application)
 '(nomad bookmark)
 '(nomad buffer)
 ;; '(nomad curl)
 '(nomad doc)
 '(nomad download)
 '(nomad commands)
 '(nomad frame)
 '(nomad html)
 '(nomad init)
 '(nomad lib)
 '(nomad minibuffer)
 '(nomad options)
 '(nomad widget)
 '(nomad repl)
 '(nomad server)
 '(nomad shroud)
 '(nomad tests)
 '(nomad text)
 '(nomad util)
 '(nomad views)
 '(nomad webview))
