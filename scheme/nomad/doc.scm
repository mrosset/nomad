;; doc --- Self-documentation for Nomad

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

(define-module (nomad doc)
  #:use-module (ice-9 documentation)
  #:use-module (emacsy command)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy klecl)
  #:use-module (emacsy self-doc)
  #:export (doc-names
            doc-object
            doc-get
            halp
            describe-symbol))

(define (doc-names module kind depth)
  (emacsy-collect-kind (resolve-module module) kind depth))

(define (doc-object module name)
  (module-ref (resolve-module module) name))

(define (doc-get module name)
  (object-documentation (doc-object module name)))

(define-interactive (help
                     #:optional (var (completing-read "Var: "
                                                      (doc-names '(nomad nomad) #f 1)
                                                      #:to-string symbol->string)))
  "Get doc."
  (message "~a" (doc-get '(nomad nomad) var)))
