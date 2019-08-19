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

(define-module (tests download)
  #:use-module (ice-9 receive)
  #:use-module (nomad download)
  ;; #:use-module (nomad curl)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:use-module (srfi srfi-64))

(define (skip-online)
  (test-skip "url-fetch")
  (test-skip "download file")
  (test-skip "hash file")
  (test-skip "file exists"))

(skip-online)

(catch 'getaddrinfo-error
  (lambda _
    (getaddrinfo "mirrors.kernel.org" "http"))
  (lambda (key code)
    ;; if we get here no network, so disable online tests
    (skip-online)))

(define (test-downloader proc dir url)
  (let ((file (string-append dir
                             //
                             (uri->filename url))))
    (when (file-exists? file)
      (delete-file file))
    ;; FIXME: maybe download handlers should return status code?. Curl
    ;; hashes fails here why?
    (test-assert "download file"
      (proc url))
    (test-equal "hash file"
      "ecbb7a2214196c57ff9340aa71458e1559abd38f6d8d169666846935df191ea7"
      (bytevector->base16-string (file-sha256 file)))
    (test-assert "file exists"
      (file-exists? file)))
  (test-equal "download file - fail"
    #f
    (proc "http://thou.shall/not/pass")))

(test-equal "url-fetch"
  "38ffd4972ae513a0c79a8be4573403edcd709f0f572105362b08ff50cf6de521"
  (receive (port thunk)
      (open-sha256-port)
    (with-output-to-port port
      (lambda _
        (url-fetch "http://bufio.org")))
    (bytevector->base16-string (thunk))))

(let ((dir "data/downloads-tests")
      (url "https://mirrors.kernel.org/gnu/hello/hello-2.9.tar.gz"))
  (when (not (file-exists? "data"))
    (mkdir "data"))
  (test-equal "filename"
    "hello-2.9.tar.gz"
    (uri->filename url))
  (with-fluid* download-directory
    dir
    (lambda _
      (test-assert "download dir exists "
        (begin (ensure-download-directory)
               (file-is-directory? dir)))
      (test-downloader http-download dir url)
      ;; (test-downloader curl-download dir url)
      )))
