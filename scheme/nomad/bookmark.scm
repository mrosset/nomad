;; bookmark --- bookmarks for Nomad

;; Copyright (C) 2019 Mike Rosset<mike.rosset@gmail.com>
;; Copyright (C) 2019-2020 Amar Singh<nly@disroot.org>

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

(define-module (nomad bookmark)
 #:use-module (emacsy emacsy)
 #:use-module (ice-9 match)
 #:use-module (ice-9 optargs)
 #:use-module (ice-9 pretty-print)
 #:use-module (ice-9 regex)
 #:use-module (nomad init)
 #:use-module (nomad platform)
 #:use-module (nomad util)
 #:use-module (nomad uri)
 #:use-module (nomad web)
 #:use-module (nomad web-mode)
 #:use-module (srfi srfi-1)
 #:use-module (srfi srfi-26)
 #:use-module (srfi srfi-9)
 #:export (alist->bookmark
           bookmark->alist
           %bookmark-file
           bookmark-find
           bookmark-init
           bookmark-list
           bookmark-ref
           make-bookmark
           add-bookmark
           read-bookmarks
           write-bookmarks
           bookmarks
           load-bookmark
           save-bookmark))

(define %bookmark-file
  (string-append %user-nomad-directory
                 // "bookmarks.scm"))

(define* (read-bookmarks #:optional file)
  (let ((bookmark-file (or file %bookmark-file)))
    (let* ((in (open-input-file bookmark-file))
           (sexp (read in)))
      (if (eof-object? sexp)
          "Use inbuilt bookmarks"
          (set! bookmarks (map alist->bookmark sexp)))
      (close-port in))))

(define* (write-bookmarks #:optional file (books bookmarks))
  (let ((bookmark-file (or file %bookmark-file)))
    (let* ((out (open-output-file bookmark-file))
           (sexp (map bookmark->alist books)))
      (pretty-print sexp out)
      (close-port out))))

(define (bookmark-init)
  (read-bookmarks))

(define-record-type <bookmark>
  (make-bookmark id contents)
  bookmark?
  (id       bookmark-id)
  (contents bookmark-contents))

(define (bookmark-ref bookmark key)
  "Return the bookmark data associated with KEY in BOOKMARK."
  (assoc-ref (bookmark-contents bookmark) key))

(define (alist->bookmark alist)
  "Convert ALIST into a <bookmark> record."
  (make-bookmark (assq-ref alist 'id)
               (assq-ref alist 'contents)))

(define (bookmark->alist bookmark)
  "Convert BOOKMARK into an alist."
  (match bookmark
    (($ <bookmark> id contents)
     `((id . ,id)
       (contents . ,contents)))))

(define bookmarks
  (map (lambda (pair)
         (make-bookmark (symbol->string (car pair)) (cdr pair)))
       %links))

(define* (add-bookmark book #:optional (books bookmarks))
  (if (bookmark? book)
      (set! books (cons book books))
      (error "Not a bookmark")))

(define-interactive (message-bookmarks)
  "Pretty prints bookmarks to echo area"
  (message "~a"
           (with-output-to-string (lambda _
                                    (pretty-print bookmarks)))))

(define* (bookmark-list #:optional books)
  (map bookmark-id (or books bookmarks)))

(define* (bookmark-find key #:optional books)
  (filter (compose (cut string-ci=? key <>) bookmark-id)
          (or books bookmarks)))

(define-interactive (load-bookmark
                     #:optional
                     (str (completing-read "Bookmark: " (map bookmark-id bookmarks))))
  "Opens bookmark by key in current buffer"
  (make-buffer <web-buffer> #:uri
            (bookmark-contents (car (bookmark-find str))))
  #t)

(define-interactive (find-bookmark)
  "Opens bookmark using completing-read in current buffer"
  (buffer-load-uri (current-buffer)
                   (bookmark-contents
                    (car (bookmark-find
                          (completing-read "Bookmark: " (bookmark-list bookmarks))
                          bookmarks)))))

(define-interactive (save-bookmark #:optional (key (read-from-minibuffer "Key: ")) (url (or (current-url) (read-from-minibuffer "URL: "))))
  "Makes a bookmark by 'KEY in a new buffer"
  (let ((book (make-bookmark key url)))
    (if bookmark? book)
    (set! bookmarks (cons book bookmarks)))
  (write-bookmarks))

;; Attempt to load user bookmarks.
(bookmark-init)
