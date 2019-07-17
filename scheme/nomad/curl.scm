;; Curl --- download things from network protocols

;; Copyright (C) 2019 Amar Singh<nly@disroot.org>

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

(define-module (nomad curl)
  #:use-module (curl)
  #:use-module (ice-9 textual-ports)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:use-module (nomad download)
  #:use-module (web response)
  #:export (curl
            curl-download))

(define (curl url)
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'url url)
    (curl-easy-setopt handle 'header #t)
    (let* ((result (curl-easy-perform handle))
           (port (open-input-string result))
           (response (read-response port)))
      (curl-easy-cleanup handle)
      response)))

(define (curl-download url)
  "Downloads URL to 'download-directory"
  (let* ((res (curl url))
         (file (download-path url))
         (out (open-output-file file))
         (in (response-body-port res))
         (status (response-code res)))
    (if (= status 200)
        (copy-to-port out in)
        #f)))
