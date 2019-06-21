;; Shroud.scm --- Shroud integration for Nomad

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

(define-module (nomad shroud)
  #:use-module (nomad eval)
  #:use-module (nomad util)
  #:use-module (ice-9 regex)
  #:use-module (shroud secret)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (shroud-database-file
            shroud-config-file
            shroud-find-password
            shroud-show-entry))

(define shroud-database-file (~/ ".config/shroud/db.gpg"))

(define shroud-config-file (~/ ".shroud"))

(define shroud-db
  (delay (load-secrets shroud-database-file)))

(define (shroud-list* config db . args)
  (map (lambda (secret)
         (secret-id secret))
       (force db)))

(define (shroud--list)
  (shroud-list* shroud-config-file
                shroud-db))

(define (shroud-find-entries text)
  "Returns a list of matches in password list"
  (filter (cut string-match text <>) (shroud--list)))

(define* (shroud-show-entry entry #:optional key)
  (let ((e (find (compose (cut string-match entry <>) secret-id)
                 (force shroud-db))))
    (if (not key) e
        (secret-ref e key))))

(define-command (shroud-find-password entry)
  "Show password/secrets entry"
  (yank-string (shroud-show-entry (car (shroud-find-entries entry)) "password")))
