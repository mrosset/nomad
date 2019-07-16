;; download.scm
;; Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad download)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:export (uri->filename
            write-port-to-file
            http-download))

(define (uri->filename url)
  (basename (uri-path (string->uri url))))

(define (write-port-to-file in-port file)
  "Writes IN-PORT to FILE path"
  (let* ((out (open-output-file file)))
    (do ((b (get-bytevector-n in-port 1024)
            (get-bytevector-n in-port 1024)))
        ((eof-object? b))
      (put-bytevector out b))
    (close-port in-port)
    (close-port out)))

(define (http-download url)
  (let* ((file (string-append (fluid-ref download-directory)
                              //
                              (uri->filename url))))
    (receive (res body-port)
        (http-request url #:decode-body? #f #:streaming? #t)
      (if (= (response-code res) 200)
          (write-port-to-file body-port file)
          #f))))
