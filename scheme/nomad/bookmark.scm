;; bookmark --- bookmarks for Nomad

;; Copyright (C) 2019 Mike Rosset<Mike.Rosset@gmail.com>
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

(define-module (nomad bookmark)
 #:use-module (emacsy emacsy)
 #:use-module (ice-9 match)
 #:use-module (ice-9 regex)
 #:use-module (ice-9 optargs)
 #:use-module (nomad webview)
 #:use-module (nomad buffer)
 #:use-module (nomad init)
 #:use-module (nomad util)
 #:use-module (srfi srfi-1)
 #:use-module (srfi srfi-26)
 #:use-module (srfi srfi-9)
 #:export (alist->bookmark
           bookmark->alist
           bookmark-file
           bookmark-find
           bookmark-init
           bookmark-list
           bookmark-ref
           make-bookmark
           read-bookmarks
           write-bookmarks
           bookmarks
           open-bookmark
           save-bookmark))

(define bookmark-file
  (string-append (fluid-ref user-nomad-directory)
                 // "bookmarks.scm"))

(define* (read-bookmarks #:optional file)
  (let ((bookmark-file (or file bookmark-file)))
    (let* ((in (open-input-file bookmark-file))
          (sexp (read in)))
      (set! bookmarks (map alist->bookmark sexp))
      (close-port in))))

(define* (write-bookmarks #:optional file)
  (let ((bookmark-file (or file bookmark-file)))
    (let* ((out (open-output-file bookmark-file))
           (sexp (map bookmark->alist bookmarks)))
      (write sexp out)
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
  (map alist->bookmark
       '(((id . "nomad")
          (contents . "https://savannah.nongnu.org/projects/nomad"))
         ((id . "nomad-git")
          (contents . "http://git.savannah.nongnu.org/cgit/nomad.git"))
         ((id . "emacsy")
          (contents . "https://savannah.nongnu.org/projects/emacsy"))
         ((id . "guilem")
          (contents . "https://www.gnu.org/software/guile/manual/html_node"))
         ((id . "emacs")
          (contents . "https://www.gnu.org/software/emacs"))
         ((id . "guile")
          (contents . "https://www.gnu.org/software/guile")))))

(define (pp-bookmarks)
  (define (print-bookmark arg)
    (format #t "~s\t~s\n" (bookmark-id arg) (bookmark-contents arg)))
  (map print-bookmark bookmarks))

(define* (bookmark-list #:optional books)
  (map bookmark-id (or books bookmarks)))

(define* (bookmark-find key #:optional books)
  (filter (compose (cut string-match key <>) bookmark-id)
          (or books bookmarks)))

(define-interactive (open-bookmark #:optional (str (read-from-minibuffer "Bookmark: ")))
  "Opens bookmark by key in current buffer"
  (make-buffer (bookmark-contents
                (car (bookmark-find str)))))

(define-interactive (save-bookmark #:optional (key (read-from-minibuffer "Key: ")) (url (or (current-url) (read-from-minibuffer "URL: "))))
  "Makes a bookmark by 'KEY in a new buffer"
  (let ((book (make-bookmark key url)))
    (if bookmark? book)
    (set! bookmarks (cons book bookmarks)))
  (write-bookmarks))
