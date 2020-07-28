;; download.scm
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

(define-module (tests download)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (nomad download)
  #:use-module (nomad init)
  #:use-module (nomad util)
  #:use-module (oop goops)
  #:use-module (unit-test))

(define-class <test-download> (<test-case>))

(define test-uri "https://mirrors.kernel.org/gnu/make/make-4.3.tar.gz")

(define online? #t)

(catch 'getaddrinfo-error
  (lambda _
    (getaddrinfo "mirrors.kernel.org" "https"))
  (lambda (key code)
    ;; if we get here no network, so disable online tests
    (set! online? #f)))

(define-method (test-procedures (self <test-download>))
  (parameterize ((%download-directory (tmpnam)))
    (assert-equal "make-4.3.tar.gz" (uri->filename test-uri))
    (assert-equal (string-append (%download-directory) "/make-4.3.tar.gz") (download-path test-uri))))

(when online?
  (define-method (test-download (self <test-download>))
   (parameterize ((%download-directory (tmpnam) ))
     (let ((file (string-append (%download-directory) "/make-4.3.tar.gz")))
       (dynamic-wind
         (lambda ()
           (ensure-directory (%download-directory))
           (assert-true (file-exists? (%download-directory))))
         (lambda ()
           (http-download test-uri)
           (assert-true (file-exists? file))
           (assert-equal "e05fdde47c5f7ca45cb697e973894ff4f5d79e13b750ed57d7b66d8defc78e19"
                         (bytevector->base16-string (file-sha256 file))))
         (lambda ()
           (when (file-exists? file)
             (delete-file file))
           (rmdir (%download-directory))))))))
