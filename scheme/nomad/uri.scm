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
  #:export (!pointer
            !uri
            %links
            %request-uri
            %uri-schemes
            <nomad-uri-handler>
            <uri-handler>
            handle-uri
            uri-arg
            valid-uri?))

(define %request-uri (make-parameter #f))

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

(define (uri-arg str)
  "Returns the @var{str} argument as a string. Given a URI of
'nomad:/describe/%load-path' it would return @var{%load-path}."
  (let* ((uri   (string->uri str))
         (path  (uri-path uri))
         (parts (split-and-decode-uri-path path)))
    (if (>= (length parts) 1)
        (string->symbol (list-ref parts (1- (length parts))))
        'none)))

(define-class <uri-handler> ()
  (pointer #:accessor     !pointer
           #:init-keyword #:pointer
           #:init-value   #f))

(define-class <nomad-uri-handler> (<uri-handler>))

(define %uri-schemes `(("nomad" . ,<nomad-uri-handler>)))

(define (valid-uri? string)
  "Returns #t if the string @var{string} is a valid URI or #f if is not."
  (if (not (string->uri string))
      #f
      #t))

(define-method (handle-uri (handler <nomad-uri-handler>)
                           uri)
  (catch #t
    (lambda _
      (let* ((html (route-view uri)))
        (unless (string? html)
          (error (format #f "HTML view is not a string. ~a" (class-of html))))
        (load-view (!pointer handler) html uri)))
    (lambda (key . vals)
      (format #t "Error: key: ~a Value: ~a" key vals))))

(define-method (handle-uri (pointer <foreign>)
                           (string-uri <string>))
  ;; FIXME: using a parameter could cause problems if multiple schemes were
  ;; being used at the same time. Views should handle URI paths as an
  ;; procedure argument?
  (parameterize ((%request-uri string-uri))
    (let* ((uri     (string->uri string-uri))
           (scheme  (symbol->string (uri-scheme uri)))
           (class   (assoc-ref %uri-schemes scheme))
           (handler (make class #:pointer pointer)))
      (handle-uri handler uri))))
