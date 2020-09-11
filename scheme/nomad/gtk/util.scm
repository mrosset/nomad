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
  #:use-module (nomad util)
  #:use-module (nomad log)
  #:use-module (nomad gtk gi)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (emacsy emacsy)
  #:use-module (nomad application)
  #:use-module (nomad gtk frame)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (yes-or-no-p
            g-run-hook
            copy-text
            get-clipboard
            enum-ref))

(define (enum-ref enum member)
  "Returns the value of @var{enum}'s @var{member}.  If the @var{enum} or
@var{member} are not found returns #f"
  (let ((ref (gi-cache-ref 'enum enum)))
    (if ref
        (assoc-ref (!enum-set ref) member)
        #f)))

(define (g-run-hook hook . args)
  "Runs @var{hook} once with @var{args} on main GLib thread."
  (g-timeout-add 50
                 (lambda _
                   (apply run-hook hook args)
                   #f)))

(define (yes-or-no-p prompt thunk)
  "Creates a Yes or No message dialog with @var{prompt}. If the Ok button is
clicked @var{thunk} is called."
  (let ((dialog (make <gtk-message-dialog>
                  #:text prompt
                  #:buttons 'ok-cancel)))
    (let* ((response (run dialog))
           (enum     (gi-cache-ref 'enum 'gtk-response-type))
           (ok       (assoc-ref (slot-ref enum 'enum-set) 'ok)))
      (when (= response ok)
        (thunk))
      (destroy dialog))))

(define (copy-text text)
  "Copies @var{text} to primary clipboard"
  (let ((clipboard (gtk-clipboard-get-default (gdk-display-get-default))))
    (gtk-clipboard-set-text clipboard text -1)))

(define (get-clipboard)
  "Return text from primary clipboard"
  (let* ((clipboard (gtk-clipboard-get-default (gdk-display-get-default))))
    (gtk-clipboard-wait-for-text clipboard)))

(define-interactive (make-frame #:optional (app (current-application)))
  "Create a new frame with @var{app}. @var{app} defaults
to (current-application)"
  (gtk-frame-new app)
  (switch-to-buffer (next-buffer))
  (co-message "New frame: ~a" (application-id)))
