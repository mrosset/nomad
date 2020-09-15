;; menu.scm
;; Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad gtk menu)
  #:use-module (srfi srfi-1)
  #:use-module (nomad gtk gi)
  #:use-module (g-golf)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad menu)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad ibuffer)
  #:use-module (nomad web-mode)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk util)
  #:duplicates (merge-generics replace warn-override-core warn first)
  #:export (<widget-web-bar>))

(define-method (make-icon-button (id <string>))
  (gtk-button-new-from-icon-name id (enum-ref 'gtk-icon-size 'small-toolbar)))

;; Menu bar
(define-class <widget-web-bar> (<gtk-header-bar>)
  (entry     #:accessor  !entry
             #:init-form (make <widget-entry>
                           #:hexpand #t
                           #:single-line-mode #t)))

(define-method (initialize (self <widget-web-bar>) args)
  (next-method)
  (let ((back    (make-icon-button "gtk-go-back"))
        (forward (make-icon-button "gtk-go-forward"))
        (buffers (make-icon-button "gtk-dnd-multiple"))
        (m-x     (make <gtk-button> #:label "M-x")))

    (set-custom-title self (!entry self))

    ;; Packing
    (pack-start self back)
    (pack-start self forward)

    (pack-end self m-x)
    (pack-end self buffers)

    ;; Signals
    (connect back 'clicked
             (lambda _
               (buffer-back (current-buffer))))
    (connect forward 'clicked
             (lambda _
               (buffer-forward (current-buffer))))
    (connect buffers 'clicked
             (lambda _
               (ibuffer))))

  (add-hook! %menu-bar-hook
             (lambda _
               (let* ((buffer   (!buffer (!entry self)))
                      (uri      (buffer-uri (current-buffer))))
                 (with-buffer buffer
                   (delete-region (point-min) (point-max))
                   (insert (webkit-uri-for-display uri)))))))

(define-method (make-menu-bar (buffer <web-buffer>))
  (make <widget-web-bar>))

(define-interactive (menu-bar-mode #:optional (buffer (current-buffer)))
   (let ((visible? (get-visible (current-menu-bar))))
    (set-visible (current-menu-bar) (not visible?))
    (if visible?
        (set! (buffer-modes buffer)
              (cons %menu-bar-mode
                    (buffer-modes buffer)))
        (set! (buffer-modes buffer)
              (delete %menu-bar-mode
                      (buffer-modes buffer)))))
  (get-visible (current-menu-bar)))
