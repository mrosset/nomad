;; frame.scm
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

(define-module (nomad gtk frame)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad frame)
  #:use-module (nomad gtk buffers)
  #:use-module (nomad gtk generics)
  #:use-module (g-golf)
  #:export (<gtk-frame>
            gtk-frame-new))

(eval-when (expand load eval)
  (gi-import "Gdk")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "Container")
              ("Gtk" . "ApplicationWindow")
              ("Gtk" . "Window")
              ("Gtk" . "Label")
              ("Gtk" . "Entry")
              ("Gtk" . "VBox")
              ("Gtk" . "Notebook")
              ("WebKit2" . "WebView")
              ("GtkSource" . "View"))))

(define emacsy-flag-map '((mod1-mask . meta)
                          (control-mask . control)
                          (mod4-mask . hyper)
                          (shift-mask . shift)))

(define (gdk-state->emacsy-flags states)
  (filter-map (lambda (state)
         (assoc-ref emacsy-flag-map state)) states))



(define-class <gtk-frame> (<nomad-frame> <gtk-application-window>)
  (container #:accessor !container)
  (box)
  (modeline)
  (minibuffer)
  (redisplay-proc #:accessor !redisplay-proc #:init-value #f))

(define-method (initialize (self <gtk-frame>) args)
  (next-method)
  (let* ((box       (make <gtk-vbox> #:spacing 0))
        (container  (make <gtk-notebook>))
        (modeline   (make <gtk-label> #:single-line-mode #f #:xalign 0))
        (minibuffer (make <gtk-entry> #:has-frame #f)))

    (slot-set! self 'container container)
    (slot-set! self 'box box)
    (slot-set! self 'modeline modeline)
    (slot-set! self 'minibuffer minibuffer)

    ;; Widget layout
    (gtk-container-add self box)
    (gtk-box-pack-start box container #t #t 0)
    (gtk-box-pack-start box modeline #f #f 0)
    (gtk-box-pack-start box minibuffer #f #f 0)

    ;; Signals
    ;;
    ;; FIXME: when using more then one frame this should not quit the
    ;; application. Currently we only support one frame.
    (connect self 'destroy
              (lambda _
               (g-application-quit (g-application-get-default))
               #t))
    (connect self 'key-press-event key-press-cb)

    ;; Idle events
    (set! (!redisplay-proc self)
                    (lambda _
                      ;; don't tick if mini buffer is being used this is
                      ;; handle by key-press-cb. otherwise completions will
                      ;; be overwritten
                      ;; FIXME: maybe have <nomad-gtk-application> handle this?
                      (unless emacsy-display-minibuffer?
                        (emacsy-tick))
                      (gtk-entry-set-text minibuffer (emacsy-message-or-echo-area))
                      (gtk-label-set-text modeline (emacsy-mode-line))
                      #t))
    (g-timeout-add 50 (!redisplay-proc self))))

(define-method (redisplay (frame <gtk-frame>))
  ((!redisplay-proc frame)))

(define (gtk-frame-new app)
  (make <gtk-frame> #:application (slot-ref app 'g-inst)))



(define (key-press-cb frame event)
  (let* ((unicode   (gdk-keyval-to-unicode (gdk-event-key:keyval event)))
         ;; FIXME:  What happens if GDK_KEY_BackSpace is not 8?
         (unichar   (if (= unicode 8) #\del  (integer->char unicode)))
         (state     (gdk-event-key:state event))
         (type      (gdk-event:type event))
         (mod-flags (gdk-state->emacsy-flags state)))
    (if (equal? type 'key-press)
        (begin
          (emacsy-key-event unichar mod-flags)
          (emacsy-tick)
          (redisplay frame)
          (if emacsy-ran-undefined-command?
              #f
              #t))
        #f)))
