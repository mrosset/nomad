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
  #:use-module (nomad widget)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk util)
  #:duplicates (merge-generics replace warn-override-core warn first)
  #:export (<widget-web-bar>))

(g-export menu-bar)

(define-method (make-icon-button (id <string>))
  (let ((btn (gtk-button-new-from-icon-name
              id
              (enum-ref 'gtk-icon-size 'small-toolbar))))
    (gtk-button-set-relief btn 'none)
    btn))

;; Web menu bar
(define-class <widget-web-bar> (<gtk-header-bar>)
  (entry     #:accessor  !entry
             #:init-form (make <widget-entry>
                           #:editable #f
                           ;; #:primary-icon-name "dialog-warning"
                           #:secondary-icon-activatable #t
                           #:hexpand #t)))

(define-method (initialize (self <widget-web-bar>) args)
  (next-method)
  (let ((back    (make-icon-button "gtk-go-back"))
        (forward (make-icon-button "gtk-go-forward"))
        (buffers (make-icon-button "gtk-dnd-multiple"))
        (m-x     (make <gtk-button>
                   #:label "M-x"
                   #:relief 'none)))

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

  (add-hook! (!menu-hook (current-buffer))
             (lambda _
               (let* ((uri  (widget-uri (current-buffer)))
                      (icon (cond
                             ((!is-loading (buffer-widget (current-buffer)))
                              #f)
                             ((secure? (current-buffer))
                              "channel-secure-symbolic")
                             (else #f))))

                 (set-icon-from-icon-name (!entry self)
                                          'primary
                                          icon)
                 (with-buffer (!buffer (!entry self))
                              (delete-region (point-min) (point-max))
                              (insert (webkit-uri-for-display uri))))))

  (show-all self))

(define-method (menu-bar (buffer <text-buffer>))
  (!menu (current-frame)))

(define-method (menu-bar (buffer <web-buffer>))
  (unless (!menu buffer)
    (set! (!menu buffer) (make <widget-web-bar>)))
  (!menu buffer))

(set-current-module (resolve-module '(nomad menu)))

(use-modules (nomad gtk frame))

(define-interactive (menu-bar-mode)
  "Toggles the current menu bar on or off."
  (let* ((visible? (get-visible (current-menu))))
    (set-visible (current-menu) (not visible?)))
  (get-visible (current-menu)))
