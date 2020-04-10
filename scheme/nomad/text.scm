;; text.scm
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

(define-module (nomad text)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<widget-buffer>
            buffer-widget))

(define-class <widget-buffer> (<buffer>)
  (widget #:accessor    buffer-widget
          #:init-value  #f)
  (add    #:init-keyword #:add
          #:init-value  #t))

(define-method (buffer-widget (buffer <text-buffer>))
  (local-var 'widget))
