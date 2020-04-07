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
  #:use-module (g-golf)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad gtk gi)
  #:use-module (nomad gtk window)
  #:use-module (nomad gtk widget)
  #:use-module (nomad web)
  #:use-module (nomad text)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<gtk-frame>
            !container
            gtk-frame-new
            current-frame))

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



(define-class <gtk-frame> (<gtk-application-window>)
  (container   #:accessor    !container
               #:init-form   (make <gtk-vbox> #:spacing 0))
  (echo-area   #:accessor    !echo-area
               #:init-form   (make <widget-thunk-view>
                               #:top-margin 1
                               #:bottom-margin 1
                               #:language "scheme"
                               #:buffer minibuffer
                               #:thunk  emacsy-message-or-echo-area))
  (window      #:accessor    !emacsy-window
               #:init-form   (make <widget-window>
                               #:window-buffer (current-buffer)))
  (show        #:accessor     !show
               #:init-keyword #:show
               #:init-value   #t))

(define-method (initialize (self <gtk-frame>) args)
  (next-method)
  (let* ((box (make <gtk-vbox> #:spacing 0)))
    ;; Initialize root window
    (set! current-window (!emacsy-window self))
    (set! root-window  (make <internal-window>
                         #:orientation 'vertical
                         #:window-children `(,current-window)))

    (nomad-set-wrap-mode (!echo-area self) #t)


    ;; Initialize slots
    ;;
    ;; FIXME: These should be set before next-method or using the make
    ;; keywords
    (slot-set! self 'title "Nomad")
    (slot-set! self 'default-height 480)
    (slot-set! self 'default-width 640)
    (slot-set! self 'icon-name "nomad")
    (gtk-window-set-icon-name self "nomad")


    ;; Add buffer hooks for minibuffer
    (add-hook! (buffer-enter-hook minibuffer)
               (lambda _
                 (grab-focus (!echo-area self))))

    (add-hook! (buffer-exit-hook minibuffer)
               (lambda _
                 (grab-focus (buffer-widget
                              (window-buffer current-window)))
                 (g-timeout-add 50
                                (lambda _
                                  (run-hook %thunk-view-hook)
                                  #f))))

    ;; Widget packing
    (gtk-container-add self box)
    (gtk-box-pack-start box (!container self) #t #t 0)
    (gtk-box-pack-start box (!echo-area self) #f #f 0)

    (connect self 'key-press-event key-press-event)

    (connect self 'focus-in-event
             (lambda (widget event)
               (set! current-window (!emacsy-window self))
               (set! root-window  (make <internal-window>
                                    #:orientation 'vertical
                                    #:window-children `(,current-window)))
               (switch-to-buffer (window-buffer current-window))
               (redisplay root-window)
               (grab-focus (buffer-widget
                            (window-buffer current-window)))
               #f))

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

(define-public (delete-frame)
  (gtk-widget-destroy (current-frame))
  #t)



(define (key-press-event frame event)
  (let* ((unicode   (gdk-keyval-to-unicode (!keyval event)))
         ;; FIXME:  What happens if GDK_KEY_BackSpace is not 8?
         (unichar   (if (= unicode 8) #\del  (integer->char unicode)))
         (state     (!state event))
         (type      (!type event))
         (mod-flags (gdk-state->emacsy-flags state)))
    (if (equal? type 'key-press)
        (begin
          (emacsy-key-event unichar mod-flags)
          (emacsy-tick)
          (run-hook %thunk-view-hook)
          ;; We need two ticks or we can not test for emacsy-ran-undefined-command?
          (unless emacsy-display-minibuffer?
            (emacsy-tick))
          (if emacsy-ran-undefined-command?
              #f
              #t))
        #f)))
