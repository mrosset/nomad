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

(define style-sheet "
table {
font-size: 14px;
border-collapse: collapse;
width: 100%;
}

.fill {
min-height: 100%;
width: 100%;
}

.grid-container {
display: grid;
grid-template-columns: auto auto auto auto;
grid-gap: 1px;
}

.grid-item {
background-color: rgba(255, 255, 255, 0.8);
text-align: left;
}

.selected {
background-color: steelblue;
color: white;
}

.key-cell {
text-align: right;
color: steelblue;
width: 2em;
font-weight: bolder;
}

.accent {
color: steelblue;
}
")

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

(define-popup-view (which-key-view-old lst selection)
  `(table ,@(map (lambda (cmd)
		   `(tr (td (@ (class "accent")) ,(car cmd)) (td ,(car (cdr cmd)))))
		 lst)))

(define-popup-view (which-key-view lst selection)
  `(div (@ (class "grid-container")) ,@(map (lambda (cmd)
				 `(div (@ (class "grid-item"))
				       (table
					(tr
					 (td (@ (class "key-cell")) ,(car cmd))
					 (td ,(car (cdr cmd)))))))
			       lst)))

(define-popup-view (completion-view lst selection)
  `(table
	 ,(let ((count 0))
	    (map (lambda (item)
		   (let ((tr `(tr (td ,item)))
			 (selected `(tr (@ (class "selected")) (td ,item))))
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
	   (div (@ (class "fill"))
		(table
		       ,(let ((count 0))
			  (map (lambda (item)
				 (let ((tr `(tr (td ,item)))
				       (selected `(tr ,tr-selected (td ,item))))
				   (when (= count selection)
				     (set! tr selected))
				   (set! count (+ count 1))
				   tr))
			       lst))))))))
