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
  #:use-module (web uri)
  #:use-module (nomad util)
  #:export (curl-download-directory
            curl
            curl-download))

(define curl-download-directory
  (make-fluid (~/ "download/")))

(define (uri->filepath url)
  (string-join (split-and-decode-uri-path
                (uri-path (string->uri url)))
               "-"))

(define (write-to-file file str)
  (let ((out (open-output-file file)))
    (put-string out str)
    (close-port out)))

(define (curl url)
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'url url)
    (let ((result (curl-easy-perform handle)))
      (curl-easy-cleanup handle)
      result)))

(define (curl-download url)
  (let ((res (curl url))
        (file (string-append (fluid-ref curl-download-directory)
                             (uri->filepath url))))
    (unless (string-null? res)
      (write-to-file file res))))
