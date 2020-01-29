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
  #:use-module (nomad gtk widgets)
  #:use-module (nomad platform api)
  #:use-module (g-golf)
  #:export (<gtk-frame>
            toggle-tabs*
            gtk-frame-new
            current-frame))

(default-duplicate-binding-handler
  '(merge-generics replace warn-override-core warn last))

(eval-when (expand load eval)
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
              ("WebKit2" . "WebView")
              ("GtkSource" . "View")
              ("GtkSource" . "StyleScheme")
              ("GtkSource" . "StyleSchemeManager")
              ("GtkSource" . "LanguageManager"))))

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
  (container #:accessor !container)
  (box #:accessor !modeline)
  (modeline)
  (minibuffer))

(define-method (initialize (self <gtk-frame>) args)
  (next-method)
  (let* ((box       (make <gtk-vbox> #:spacing 0))
         (container  (make <gtk-notebook>))
         (modeline   (make <widget-source-view>
                       #:theme "cobalt"
                       #:top-margin 1
                       #:bottom-margin 1
                       #:thunk emacsy-mode-line))
         (mini-view  (make <widget-source-view>
                       #:top-margin 1
                       #:bottom-margin 1
                       #:buffer minibuffer
                       #:parent self
                       #:thunk  emacsy-message-or-echo-area)))

    (slot-set! self 'container container)
    (slot-set! self 'box box)
    (slot-set! self 'modeline modeline)
    (slot-set! self 'minibuffer mini-view)

    ;; Widget styles
    (nomad-app-set-style (slot-ref self 'modeline) "textview text { background-color: #BFBFBF; color: black; }")
    (nomad-app-set-style (slot-ref self 'minibuffer) "textview text { background-color: white; color: black; }")

    ;; Widget layout
    (gtk-container-add self box)
    (gtk-box-pack-start box container #t #t 0)
    (gtk-box-pack-start box modeline #f #f 0)
    (gtk-box-pack-start box mini-view #f #f 0)

    ;; Signals
    ;;
    ;; FIXME: when using more then one frame this should not quit the
    ;; application. Currently we only support one frame.
    (connect self 'destroy
             (lambda _
               (g-application-quit (g-application-get-default))
               #t))
    (connect self 'key-press-event key-press-cb)))

(define-method (toggle-tabs* (self <gtk-frame>))
  (let ((notebook (!container self)))
    (when (eq? <gtk-notebook> (class-of notebook))
      (gtk-notebook-set-show-tabs  notebook (not
                                             (gtk-notebook-get-show-tabs notebook))))))

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
          (if emacsy-ran-undefined-command?
              #f
              #t))
        #f)
    (when (equal? type 'key-release)
      #f)))
