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
height: 100vh;
width: 100%;
")))

(define tr-selected-style `(@ (style "
background-color: steelblue;
color: white;
")))

(define table-style
  `(@ (style "
font-size: 14px;
border-collapse: collapse;
width: 100%;
")
      (cellpadding "0")))

(define-public (completion-view)
  (let ((count 0))
    (sxml->html-string
     `(html
       (head
	(title "completion view"))
       (body (@ (style "margin: 0px 0px 0px 0px;"))
	     (div ,fill-style
		  (table ,table-style
			 ,@(map (lambda (proc) (version)
					(let ((tr ""))
					  (if (= count current-selection)
					      (set! tr `(tr ,tr-selected-style (td ,proc)))
					      (set! tr `(tr (td ,proc))))
					  (set! count (+ count 1))
					  tr))
				(input-completion current-input)))))))))
