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
  #:use-module (nomad util)
  #:use-module (nomad gtk util)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module (shroud secret)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (shroud-database-file
            shroud-config-file
            shroud-find-password
            shroud-show-entry))

;; The file where Shroud stores secrets.
(define shroud-database-file (~/ ".config/shroud/db.gpg"))

;; Shroud configuration file
(define shroud-config-file (~/ ".shroud"))

(define-macro (dynamic x)
  `(eval ,x (interaction-environment)))

(define (shroud-list config db . args)
  (map (lambda (secret)
         (secret-id secret))
       (force db)))

;; Find matching entries for a given string
(define (shroud-find-entries text)
  "Returns a list of matches in password list"
  (filter (cut string-match text <>)
          (shroud-list (dynamic shroud-config-file)
                       (delay (load-secrets (dynamic shroud-database-file))))))

(define* (shroud-show-entry entry #:optional key)
  (let ((e (find (compose (cut string-match entry <>) secret-id)
                 (force (delay (load-secrets (dynamic shroud-database-file)))))))
    (if (not key) e
        (secret-ref e key))))

(define-interactive
  (shroud-find-password
   #:optional (entry (completing-read "Entry: "
                                      (shroud-list (dynamic shroud-config-file)
                                                   (delay (load-secrets
                                                           (dynamic shroud-database-file)))))))
  "Show password/secrets entry"
  (copy-text (shroud-show-entry entry "password")))
