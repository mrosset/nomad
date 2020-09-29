;; widget.scm
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

(define-module (nomad widget)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<widget-buffer>
            !menu
            !menu-hook
            buffer-widget))

(define-class <widget-buffer> (<buffer>)
  (menu      #:accessor   !menu
             #:init-value #f)
  (menu-hook #:accessor   !menu-hook
             #:init-form  (make-hook))
  (widget #:accessor   buffer-widget
          #:init-value  #f))

(define-method (buffer-widget (buffer <text-buffer>))
  (local-var 'widget))

(define-method (buffer-widget (buffer <minibuffer>))
  #f)
