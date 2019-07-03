;; Self-doc --- Self-documentation for Nomad

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

(define-module (nomad self-doc)
  #:use-module (ice-9 documentation)
  #:use-module (emacsy emacsy)
  #:use-module (nomad webkit)
  #:use-module (nomad buffer)
  #:export (describe-symbol))

(define-interactive (describe-symbol #:optional (var (read-from-minibuffer "Variable: ")))
  "Open variable documentation in new buffer."
  (make-buffer "*help*")
  (webview-load-content (object-documentation
                         (eval (string->symbol var)
                               (interaction-environment)))
                        "*help*"))
