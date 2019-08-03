;; webkit-settings --- configure webkit renderer.

;; Copyright (C) 2019  Amar Singh

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

(define-module (nomad webkit-settings)
  #:use-module (nomad lib)
  #:use-module (srfi srfi-26)
  #:export (make-webkit-settings
            webkit-settings-default))

(load-extension (dynamic-path) "init_guile_nomad_webkitsettings")

(define webkit-settings-old webkit-settings-new)

(define (make-webkit-settings lst)
  " Usage example:
(define my-webkit-settings
 '((webkit-settings-set-auto-load-images #t)))
(make-webkit-settings my-webkit-settings) "
  (let* ((settings (webkit-settings-old))
        (module (resolve-module '(nomad webkit-settings)))
        (setter (lambda (pair settings)
                  (apply (module-ref module (car pair)) settings (cdr pair)))))
    (for-each (cut setter <> settings)
         lst)
    settings))

(define webkit-settings-default
  '((webkit-settings-set-enable-javascript #t)))

(define* (webkit-settings-new #:optional #:key webkit-settings)
  "webkit-settings is an alist of setters and values."
  (make-webkit-settings webkit-settings-default))
(export webkit-settings-new)
