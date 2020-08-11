;; views.scm
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

(define-module (nomad views)
  #:use-module (nomad application)
  #:use-module (emacsy emacsy)
  #:use-module (nomad buffer)
  #:use-module (nomad util)
  #:use-module (nomad html)
  #:export (restful-view
            %nomad-restful-views))

(define-syntax define-popup-view
  (syntax-rules ()
    ((define-view (proc  ...) thunk)
     (define-public (proc ...)
       (sxml->html-string
        `(html
          (style ,style-sheet)
          (head
           (title "nomad view"))
          (body (@ (onload "document.getElementById('selected').scrollIntoView();")
                   (style "margin: 0px 0px 0px 0px;"))
                (div (@ (class "fill"))
                     ,thunk))))))))

(define (about-view)
  (sxml->html-string
   `(html
     (head
      (title "About Nomad"))
     (body
      (h1 "About Nomad ")))))

(define (404-view)
  (sxml->html-string
   `(html
     (head
      (title "View not found"))
     (body
      (h1 "404 view not found ")))))

(use-modules (g-golf)
             (oop goops))

(define (restful-ref views path)
  "Returns a sxml @var{path} from a alist @var{views}"
  (let ((view (assoc-ref views path)))
    (if view
        view
        404-view)))

(define (restful-view path)
  (let ((view (restful-ref %nomad-restful-views path)))
    (view)))

(define %nomad-restful-views `(("about" . ,about-view)))
