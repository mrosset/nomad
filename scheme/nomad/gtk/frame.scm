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
  #:use-module (emacsy window)
  #:use-module (nomad gtk window)
  #:use-module (nomad gtk widget)
  #:use-module (nomad web)
  #:use-module (nomad text)
  #:use-module (nomad api)
  #:use-module (g-golf)
  #:export (<gtk-frame>
            !root
            gtk-frame-new
            current-frame))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "Container")
              ("Gtk" . "ApplicationWindow")
              ("Gtk" . "Window")
              ("Gtk" . "Label")
              ("Gtk" . "Entry")
              ("Gtk" . "VBox")
              ("Gtk" . "Notebook")
              ("Gtk" . "CssProvider")
              ("Gtk" . "StyleContext")
              ("Gtk" . "Overlay")
              ("Gtk" . "Grid")
              ("WebKit2" . "WebView")
              ("GtkSource" . "View")
              ("GtkSource" . "StyleScheme")
              ("GtkSource" . "StyleSchemeManager")
              ("GtkSource" . "LanguageManager"))))

(define-public (current-popup)
  "Returns the current frame"
  (!mini-popup (current-frame)))

(define-public (current-frame)
  "Returns the current frame"
  (let* ((app (g-application-get-default))
         (frame (gtk-application-get-active-window app)))
    frame))

(define emacsy-flag-map '((mod1-mask . meta)
                          (control-mask . control)
                          (mod4-mask . hyper)
                          (shift-mask . shift)))

(define (gdk-state->emacsy-flags states)
  (filter-map (lambda (state)
         (assoc-ref emacsy-flag-map state)) states))



(define-class <gtk-frame> (<nomad-frame> <gtk-application-window>)
  (root        #:accessor    !root
               #:init-form   (make <gtk-vbox> #:spacing 0))
  (echo-area   #:accessor    echo-area
               #:init-form   (make <widget-text-view>
                               #:top-margin 1
                               #:bottom-margin 1
                               #:buffer minibuffer
                               #:thunk  emacsy-message-or-echo-area))
  (show        #:accessor     !show
               #:init-keyword #:show
               #:init-value   #t))

(define-method (initialize (self <gtk-frame>) args)
  (next-method)
  (let* ((box (make <gtk-vbox> #:spacing 0)))
    ;; Initialize root window
    (set! current-window (make <widget-window>
                           #:window-buffer (current-buffer)))
    (set! root-window  (make <internal-window>
                         #:orientation 'vertical
                         #:window-children (list current-window)))


    ;; Initialize slots
    (slot-set! self 'title "Nomad")
    (slot-set! self 'default-height 480)
    (slot-set! self 'default-width 640)
    (slot-set! self 'icon-name "nomad")
    (gtk-window-set-icon-name self "nomad")


    ;; Add buffer hooks for minibuffer
    (add-hook! (buffer-enter-hook minibuffer)
               (lambda _
                 (grab-focus (echo-area self))))

    (add-hook! (buffer-exit-hook minibuffer)
               (lambda _
                 (grab-focus (buffer-widget
                              (window-buffer current-window)))))

    ;; Widget packing
    (gtk-container-add self box)
    (gtk-box-pack-start box (!root self) #t #t 0)
    (gtk-box-pack-start box (echo-area self) #f #f 0)

    ;; FIXME: when using more then one frame this should not quit the
    ;; application. Currently we only support one frame.
    (connect self 'destroy
             (lambda _
               (g-application-quit (g-application-get-default))
               #t))

    (connect self 'key-press-event key-press-event)


    ;; Redraw the windows for the first time.
    (window-config-change root-window)

    (g-timeout-add 200 (lambda _
                        (redisplay root-window)
                        #t))

    (when (!show self)
      (show-all self))))

(define (gtk-frame-new app)
  (make <gtk-frame> #:application (slot-ref app 'g-inst)
        #:icon-name "nomad"))



(define (key-press-event frame event)
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
          ;; We need two ticks or we can not test for emacsy-ran-undefined-command?
          (unless emacsy-display-minibuffer?
            (emacsy-tick))
          (if emacsy-ran-undefined-command?
              #f
              #t))
        #f)))
