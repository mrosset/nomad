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
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (nomad util)
  #:use-module (nomad log)
  #:export (%download-directory
            allow-downloads?
            uri->filename
            url-fetch
            copy-to-port
            download-path
            download-function
            http-download))

;; The directory where downloads should be saved to.
(define %download-directory (make-parameter (~/ "downloads")))

(define (uri->filename uri)
  "Returns the filename at the end of @var{uri}"
  (basename (uri-path (string->uri uri))))

(define (download-path url)
  "Returns full path of URI download location"
  (string-append (%download-directory)
                 //
                 (uri->filename url)))

(define (download-exists? url)
  "Returns #t if download already exists"
  (file-exists? (download-path url)))

(define (copy-to-port out in)
  "Writes all of IN port to OUT port. Ports are closed when procedure returns"
  (do ((b (get-bytevector-n in 16384)
          (get-bytevector-n in 16384)))
      ((eof-object? b))
    (put-bytevector out b))
  (close-port in)
  (close-port out))

(define (url-fetch url)
  "Retrieves URL synchronously and writes to current output port. If the http
request fails it will return the http status code."
  (receive (res body)
      (http-request url #:decode-body? #f#:streaming? #t)
    (if (= (response-code res) 200)
        (begin (copy-to-port (current-output-port)
                     body)
               #t)
        (response-code res))))

(define (http-download url)
  "Downloads URL to 'download-directory"
  (let* ((file (download-path url))
         (out (open-output-file file)))
    (receive (res in)
        (http-request url #:decode-body? #f #:streaming? #t)
      (if (= (response-code res) 200)
          (copy-to-port out in)
          #f))))

(define allow-downloads? #f)

(define download-function
  (lambda (url)
    (if (download-exists? url)
        (co-message "download ~a exists.. skipping" url)
        (begin (co-message "downloading ~a" url)
               (http-download url)
               (co-message "download complete ~a" url)))))

(define-interactive (nomad-download referrer #:optional (url (read-from-minibuffer "Url: ")))
  (if allow-downloads?
      (download-function url)
      (co-message "downloads are not allowed. You can enable by setting 'allow-downloads? to #t")))
;; (let ((confirm (co-read-from-minibuffer (format #f
;;                                              "~s is requesting to download ~s continue: y/n? "
;;                                              referrer
;;                                              url))))
;;   (when (and (string= confirm "y") allow-downloads?)
;;     (download-function url)))
