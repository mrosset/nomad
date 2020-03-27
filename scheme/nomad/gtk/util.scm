;; util.scm
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

(define-module (nomad gtk util)
  #:use-module (g-golf))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))
  (gi-import "GdkX11")
  (map (lambda (pair)
         (gi-import-by-name (car pair) (cdr pair)))
       '(("Gtk" . "Clipboard")
         ("Gdk" . "Display"))))

(define-public (copy-text text)
  "Copies @var{text} to primary clipboard"
  (let ((clipboard (gtk-clipboard-get-default (gdk-display-get-default))))
    (gtk-clipboard-set-text clipboard text -1)))

(define-public (get-clipboard)
  "Return text from primary clipboard"
  (let* ((clipboard (gtk-clipboard-get-default (gdk-display-get-default))))
    (gtk-clipboard-wait-for-text clipboard)))
