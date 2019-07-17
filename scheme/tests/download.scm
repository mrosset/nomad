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

(define-module (tests eval)
  #:use-module (nomad download)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:use-module (srfi srfi-64))

(define (test-downloader proc dir url)
  (test-assert "download dir exists "
    (begin (ensure-download-directory)
           (file-is-directory? dir)))
  (let ((file (string-append dir
                             //
                             (uri->filename url))))
    (when (file-exists? file)
      (delete-file file))
    ;; FIXME: maybe download handlers should return status code?  also after
    ;; downloading hash the file. Curl fails here why?
    (test-assert "download file - pass"
      (proc url))
    (test-assert "file exists"
      (file-exists? file)))
  (test-equal "download file - fail"
    #f
    (proc "http://thou.shall/not/pass")))

(test-begin "downloaders")

(let ((dir "data/downloads-tests")
      ;; TODO: use https
      (url "https://mirrors.kernel.org/gnu/hello/hello-2.9.tar.gz"))
  (test-equal "filename"
    "hello-2.9.tar.gz"
    (uri->filename url))
  (with-fluid* download-directory
    dir
    (lambda _
      ;; (test-downloader curl-download dir url)
      (test-downloader http-download dir url)
      )))

(test-end)
