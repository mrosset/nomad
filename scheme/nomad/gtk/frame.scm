;; frame.scm
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

(define-module (nomad gtk frame)
  #:use-module (nomad gtk webview)
  #:use-module (nomad gtk generics)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (emacsy emacsy)
  #:export (<gtk-frame>
            gtk-frame-new))

(eval-when (expand load eval)
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "Container")
              ("Gtk" . "ApplicationWindow")
              ("Gtk" . "Window")
              ("Gtk" . "Label")
              ("Gtk" . "Entry")
              ("Gtk" . "VBox")
              ("WebKit2" . "WebView")
              ("GtkSource" . "View"))))

(define-class <gtk-frame> (<gtk-application-window>)
  (box)
  (container)
  (view)
  (modeline)
  (minibuffer))

(use-modules (nomad frame))

(define (key-press-cb frame)
  #f)

(define-public redisplay #f)

(define-method (initialize (self <gtk-frame>) args)
  (next-method)
  (let ((box (make <gtk-vbox> #:spacing 0))
        (view       (make <nomad-gtk-webview>))
        (mode-fmt   "<span background=\"DarkGray\">~a</span>")
        (modeline   (make <gtk-label> #:single-line-mode #f #:xalign 0))
        (minibuffer (make <gtk-entry> #:has-frame #f)))

    (slot-set! self 'box box)
    (slot-set! self 'view view)
    (slot-set! self 'modeline modeline)
    (slot-set! self 'minibuffer minibuffer)

    ;;
    ;; widget layout
    (gtk-container-add self box)
    (gtk-box-pack-start box view #t #t 0)
    (gtk-box-pack-start box modeline #f #f 0)
    (gtk-box-pack-start box minibuffer #f #f 0)

    ;; signals
    (connect self 'destroy
             (lambda _
               (g-application-quit (g-application-get-default))
               #f))

    (nomad-app-frame-setup-keypress self)
    ;; (connect self 'key-press-event (lambda (x) #t))

    ;; idle events
    ;; (g-timeout-add 50 emacsy-tick)
    (set! redisplay (lambda _
                             (gtk-entry-set-text minibuffer (emacsy-message-or-echo-area))
                             (gtk-label-set-text modeline (emacsy-mode-line))
                             #t))

    (g-timeout-add 50 redisplay)))

(define (key-press-cb frame)
  #t)

(define (gtk-frame-new app)
  (make <gtk-frame> #:application (slot-ref app 'g-inst)))
