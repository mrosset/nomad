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
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<widget-menu-bar>))

(define-method (make-menu-button (id <string>))
  (gtk-button-new-from-icon-name id (enum-ref 'gtk-icon-size 'small-toolbar)))

;; Menu box
(define-class <title-box> (<gtk-stack>)
  (entry #:accessor !entry
         #:init-form (make <widget-entry>
                       #:hexpand #t
                       #:single-line-mode #t)))

(define-method (initialize (self <title-box>) args)
  (next-method)
  (add self (!entry self))
  (show-all self))

(define-method (set-text (self <title-box>)
                         (text <string>))
  (set-text (!entry self) text))

;; Menu bar
(define-class <widget-menu-bar> (<gtk-header-bar>)
  (title-box #:accessor  !title-box
            #:init-form (make <title-box>)))

(define-method (initialize (self <widget-menu-bar>) args)
  (next-method)
  (add-hook! %menu-bar-hook
             (lambda _
               (let ((buffer (!buffer (!entry (!title-box self))))
                     (url    (buffer-uri (current-buffer))))
                 (with-buffer buffer
                   (delete-region (point-min) (point-max))
                   (insert url)))))
  (let ((back    (make-menu-button "gtk-go-back"))
        (forward (make-menu-button "gtk-go-forward"))
        (buffers (make-menu-button "gtk-dnd-multiple"))
        (m-x     (make <gtk-button> #:label "M-x")))

    (set-custom-title self (!title-box self))

    ;; Packing
    ;; (gtk-menu-shell-append self menu)
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
               (ibuffer)))
    (connect m-x 'clicked
             (lambda _
               (catch #t
                 (lambda _
                   (emacsy-key-event #\x '("alt"))
                   (emacsy-tick)
                   (run-hook %thunk-view-hook))
                (lambda (key . vals)
                  (co-message "Error: key: ~a Value: ~a" key vals)))))))

(define-interactive (menu-bar-mode)
  (let ((visible? (get-visible (current-menu-bar))))
    (set-visible (current-menu-bar) (not visible?)))
  (get-visible (current-menu-bar)))
