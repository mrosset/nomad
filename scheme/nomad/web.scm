;; web.scm
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

(define-module (nomad web)
  #:use-module (srfi srfi-26)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (g-golf)
  #:export (<web-buffer>
            buffer-widgets
            buffer-widget
            current-widget
            buffer-progress
            buffer-title
            buffer-uri))

(define %default-home-page "https://www.gnu.org/software/guile")

(define (ensure-protocol url)
  "Returns a full protocol URI for domain URI.
e.g. (prefix-url \"gnu.org\") returns \"https://gnu.org\""
  (if (string-contains url "://")
      url
      (string-append "https://" url)))



;; Cache of buffer widgets these widgets are reused when possible.
;; (define-class <widget-cache> (<pair>))

;; We only have one cache %widget-cache
(define-public %widget-cache '())

(define-method (buffer-widgets (buffer <buffer>))
  "Returns the widgets for @var{buffer}"
  (filter-map  (lambda (pair)
                 (if (eq? buffer (car pair))
                     (cdr pair)
                     #f))
               %widget-cache))



(define-method (current-widget (buffer <buffer>)))

(define-class <web-buffer> (<text-buffer>)
  (widget   #:accessor     buffer-widget
            #:init-value   #f)
  (name     #:init-keyword #:name #:init-value "<web-buffer>")
  (progress #:accessor     buffer-progress
            #:init-value   0)
  (title    #:accessor     buffer-title
            #:init-value   "")
  (uri      #:accessor     buffer-uri
            #:init-keyword #:uri
            #:init-value   %default-home-page))

(define-method (initialize (self <web-buffer>) args)
  (next-method)
  (with-buffer self
    (set! (local-var 'widget) #f))
  (add-buffer! self)
  (switch-to-buffer self))





(set! buffer-classes (cons* <web-buffer>
                            buffer-classes))
