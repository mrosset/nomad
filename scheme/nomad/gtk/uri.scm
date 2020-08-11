;; uri.scm
;; Copyright (C) 2017-2020 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad gtk uri)
  #:use-module (nomad gtk gi)
  #:use-module (nomad log)
  #:use-module (nomad uri)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (register-uri-handlers))

(define (register-uri-handlers)
  (for-each (match-lambda
              ((scheme . handler)
               (log-debug "Registering uri scheme:" scheme)
               (nomad-register-uri-scheme (webkit-web-context-get-default) scheme)))
            %uri-schemes))
