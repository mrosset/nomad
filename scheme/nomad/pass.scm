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

(define-module (nomad pass))

(use-modules
 (nomad eval)
 (ice-9 regex)
 (ice-9 session)
 (shroud ui list)
 (shroud ui show)
 (shroud secret))

;; We don't alter the database.
(define (shroud-list* config db . args)
  (let* ((db   (force db)))
    (map (lambda (secret)
                (secret-id secret))
              db)))

(define shroud-db
  (delay (shroud-list* "/home/nly/.shroud"
		       (delay (load-secrets "/home/nly/.config/shroud/db-old.gpg")))))

(define (pass-completion text)
  "Returns a list of matches in password list"
  (let ((completion '()))
    (map (lambda (s)
	   (when (string-match text s)
	     (set! completion (append completion (list s)))))
	 (force shroud-db))
    completion))

(define (show-entry entry)
  (shroud-show "/home/nly/.shroud"
	       (delay (load-secrets "/home/nly/.config/shroud/db-old.gpg"))
	       entry "--clipboard" "password"))

(define-command (find-entry entry)
  "Show password/secrets entry"
  (map show-entry (pass-completion entry)))
