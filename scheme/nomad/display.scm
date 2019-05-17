;; Copyright (C) 2019  Amar Singh

;; Nomad --- An extensible web browser

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

(define-module (nomad display))

(use-modules (pict)
             (sxml simple)
             (nomad webview))
;;; pict in nomad
(define-public (pict->string record)
  (with-output-to-string
    (lambda _ (sxml->xml `(svg (@ (width ,(pict-width record))
                                  (height ,(pict-height record))
                                  (xmlns "http://www.w3.org/2000/svg"))
                               ,(pict-sxml record))))))

(define (display-pict p) (webview-load-string (pict->string p)))

;; generalize webview-load-string to display scheme values
(define-macro (buffer-display value)
  `(and (make-buffer "out")
        (webview-load-string (with-output-to-string (lambda _ (display ,value))))))

(define-macro (nomad-display value)
  `(webview-load-string (with-output-to-string (lambda _ (display ,value)))))

(export-syntax buffer-display nomad-display)
