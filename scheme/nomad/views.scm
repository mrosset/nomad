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
  #:use-module (nomad app)
  #:use-module (nomad buffer)
  #:use-module (nomad minibuffer)
  #:use-module (nomad eval)
  #:use-module (nomad html))

(define fill-style `(@ (style "
height: 99vh;
width: 100%;
border-top:1px solid steelblue;
border-bottom:1px solid steelblue;
")))

(define tr-selected `(@ (style "
background-color: steelblue;
color: white;
")))

(define accent
  `(@ (style "
color: steelblue")))

(define table-style
  `(@ (style "
font-size: 14px;
border-collapse: collapse;
width: 100%;
")
      (cellpadding "0")))

(define grid-container `(@ (style "
display: grid;
grid-template-columns: auto auto auto auto auto;
grid-gap: 1px;
")))

(define grid-item `(@ (style "
background-color: rgba(255, 255, 255, 0.8);
text-align: left;
")))

(define align-right `(@ (style "
text-align: left;
")))

(define-syntax define-view
  (syntax-rules ()
    ((define-view (proc  ...) thunk)
     (define-public (proc ...)
       (sxml->html-string
	`(html
	  (head
	   (title "nomad view"))
	  (body (@ (style "margin: 0px 0px 0px 0px;"))
		(div ,fill-style
		     ,thunk))))))))

(define-view (which-key-view-old lst selection)
  `(table ,table-style
	  ,@(map (lambda (cmd)
		   `(tr (td ,accent ,(car cmd)) (td ,(car (cdr cmd)))))
		 lst)))

(define-view (which-key-view lst selection)
  `(div ,grid-container ,@(map (lambda (cmd)
				 `(div ,grid-item
				       (font (@ (color "steelblue")) ,(car cmd)) " -> "
				       ,(car (cdr cmd))))
			       lst)))

(define-view (completion-view lst selection)
  `(table ,table-style
	 ,(let ((count 0))
	    (map (lambda (item)
		   (let ((tr `(tr (td ,item)))
			 (selected `(tr ,tr-selected (td ,item))))
		     (when (= count selection)
		       (set! tr selected))
		     (set! count (+ count 1))
		     tr))
		 lst))))

(define-public (completion-view-old lst selection)
  (sxml->html-string
   `(html
     (head
      (title "completion view"))
     (body (@ (style "margin: 0px 0px 0px 0px;"))
	   (div ,fill-style
		(table ,table-style
		       ,(let ((count 0))
			  (map (lambda (item)
				 (let ((tr `(tr (td ,item)))
				       (selected `(tr ,tr-selected (td ,item))))
				   (when (= count selection)
				     (set! tr selected))
				   (set! count (+ count 1))
				   tr))
			       lst))))))))
