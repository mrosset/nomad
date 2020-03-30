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
  #:use-module (nomad api)
  #:use-module (nomad application)
  #:use-module (nomad bookmark)
  #:use-module (nomad buffer)
  #:use-module (nomad ibuffer)
  #:use-module (nomad terminal)
  ;; #:use-module (nomad curl)
  #:use-module (nomad doc)
  #:use-module (nomad download)
  #:use-module (nomad frame)
  #:use-module (nomad html)
  #:use-module (nomad init)
  #:use-module (nomad options)
  #:use-module (nomad repl)
  #:use-module (nomad server)
  #:use-module (nomad shroud)
  #:use-module (nomad text)
  #:use-module (nomad util)
  #:use-module (nomad views)
  #:use-module (nomad web)
  #:use-module (nomad web-mode)
  #:use-module (nomad graph)
  #:duplicates (merge-generics))

(re-export-modules
 '(nomad api)
 '(nomad application)
 '(nomad bookmark)
 '(nomad buffer)
 '(nomad ibuffer)
 '(nomad terminal)
 ;; '(nomad curl)
 '(nomad doc)
 '(nomad download)
 '(nomad frame)
 '(nomad html)
 '(nomad init)
 '(nomad options)
 '(nomad repl)
 '(nomad server)
 '(nomad shroud)
 '(nomad text)
 '(nomad util)
 '(nomad views)
 '(nomad web)
 '(nomad web-mode)
 '(nomad graph))
