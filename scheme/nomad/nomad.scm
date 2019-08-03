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
  #:use-module (nomad app)
  #:use-module (nomad bookmark)
  #:use-module (nomad buffer)
  ;; #:use-module (nomad curl)
  #:use-module (nomad doc)
  #:use-module (nomad download)
  #:use-module (nomad eval)
  #:use-module (nomad frame)
  #:use-module (nomad html)
  #:use-module (nomad init)
  #:use-module (nomad lib)
  #:use-module (nomad minibuffer)
  #:use-module (nomad options)
  #:use-module (nomad pointer)
  #:use-module (nomad repl)
  #:use-module (nomad server)
  #:use-module (nomad shroud)
  #:use-module (nomad tests)
  #:use-module (nomad text)
  #:use-module (nomad util)
  #:use-module (nomad views)
  #:use-module (nomad webkit-proxy)
  #:use-module (nomad webkit-settings)
  #:use-module (nomad webkit)
  #:use-module (nomad webview))

;;; Taken from Emacsy
(define (re-export-modules . modules)
  "Re-export modules"
  (define (re-export-module module)
    (module-for-each
     (lambda (sym var)
       ;;(format #t "re-exporting ~a~%" sym)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))

(re-export-modules
 '(nomad app)
 '(nomad bookmark)
 '(nomad buffer)
 ;; '(nomad curl)
 '(nomad doc)
 '(nomad download)
 '(nomad eval)
 '(nomad frame)
 '(nomad html)
 '(nomad init)
 '(nomad lib)
 '(nomad minibuffer)
 '(nomad options)
 '(nomad pointer)
 '(nomad repl)
 '(nomad server)
 '(nomad shroud)
 '(nomad tests)
 '(nomad text)
 '(nomad util)
 '(nomad views)
 '(nomad webkit-proxy)
 '(nomad webkit-settings)
 '(nomad webkit)
 '(nomad webview))
