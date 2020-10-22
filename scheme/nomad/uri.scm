;; uri.scm
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

(define-module (nomad uri)
  #:use-module (nomad views)
  #:use-module (nomad util)
  ;; FIXME: we should not use GTK from here make this generic
  #:use-module (nomad gtk foreign)
  #:use-module (web uri)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (%links
            !uri
            <uri-handler>
            <nomad-uri-handler>
            %uri-schemes
            handle-uri
            valid-uri?))

(define %links '((nomad        . "https://www.nongnu.org/nomad")
                 (nomad-git    . "https://git.savannah.nongnu.org/cgit/nomad.git")
                 (gnu          . "https://gnu.org")
                 (guile        . "https://www.gnu.org/software/guile")
                 (guix         . "https://guix.gnu.org")
                 (guix-git     . "https://git.savannah.gnu.org/cgit/guix.git")
                 (emacs        . "https://www.gnu.org/software/emacs")
                 (emacsy       . "https://savannah.nongnu.org/projects/emacsy")
                 (guile-manual . "https://www.gnu.org/software/guile/manual/html_node")
                 (guix-manual  . "https://guix.gnu.org/manual/devel/")))

(define-class <uri-handler> ()
  (pointer #:accessor    pointer
           #:init-keyword #:pointer
           #:init-value  #f)
  (uri     #:accessor    !uri
           #:init-keyword #:uri
           #:init-value #f))

(define-class <nomad-uri-handler> (<uri-handler>))

(define %uri-schemes `(("nomad" . ,<nomad-uri-handler>)))

(define (valid-uri? string)
  "Returns #t if the string @var{string} is a valid URI or #f if is not."
  (if (not (string->uri string))
      #f
      #t))

(define-method (handle-uri (handler <nomad-uri-handler>))
  (catch #t
    (lambda _
      (let* ((path (uri-path (!uri handler)))
             (html (route-view path)))
        (unless (string? html)
          (error (format #f "HTML view is not a string. ~a" (class-of html))))
        (load-view (pointer handler) html (!uri handler))))
    (lambda (key . vals)
      (format #t "Error: key: ~a Value: ~a" key vals))))

(define-method (handle-uri (pointer <foreign>) (string-uri <string>))
  (let* ((uri     (string->uri string-uri))
         (scheme  (symbol->string (uri-scheme uri)))
         (class   (assoc-ref %uri-schemes scheme))
         (handler (make class #:pointer pointer #:uri uri)))
    (handle-uri handler)))
