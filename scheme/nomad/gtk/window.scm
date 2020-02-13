;; window.scm
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

(define-module (nomad gtk window)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk buffers)
  #:use-module (g-golf)
  #:export (<nomad-gtk-window>
            redisplay
            remove-user-data
            !widget
            !last-tick
            !last-buffer
            !container))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (gi-import "GLib")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "Box")
              ("Gtk" . "HBox")
              ("Gtk" . "Expander"))))


(define-method (instantiate-window (window <window>))
  ;; window
  ;; (describe window)
  ;; (let ((buffer (window-buffer window)))
  ;;   (create-web-view-window window buffer (is-a? buffer <text-buffer>)))
  )

(define (create-window list orientation)
  (let ((box (make <gtk-hbox>)))
    (for-each
     (lambda (window)
       (dimfi window)
       ;; (let ((widget (!widget window)))
       ;;   (dimfi (!widget window))
       ;;   ;; (if (gtk-widget-get-parent widget)
       ;;   ;;     (gtk-widget-reparent widget box)
       ;;   ;;     (gtk-container-add box widget))
       ;;   )
       )
     list)
    (gtk-widget-show-all box)
    box))

(define-method (instantiate-window (window <internal-window>))
  ;; (let* ((windows (filter-map instantiate-window
  ;;                     (window-children window)))
  ;;        (box     (create-window windows #t))
  ;;        (root    (!root (current-frame))))
  ;;   (gtk-container-remove root
  ;;                         (dimfi (container-child root)))
  ;;   (gtk-container-add root box)
  ;;   ;; ( (!root (current-frame)) box)
  ;;   )
  #t)

(define (window-change internal-window)
  (instantiate-window root-window))

;; (add-hook! window-configuration-change-hook window-change)

(define-class <nomad-gtk-window> (<window>)
  (last      #:accessor   !last-buffer)
  (widget    #:accessor   !widget
             #:init-form  (make <gtk-scrolled-window>))
  (container #:accessor   !container
             #:init-form  (make <gtk-vbox>))
  (last-tick #:accessor   !last-tick
             #:init-value -1)
  (mode-line #:accessor   !mode-line
             #:init-form  (make <widget-source-view>
                           #:top-margin 1
                           #:bottom-margin 1
                           #:thunk emacsy-mode-line)))


;; Redisplay current window on each event read. This is mainly used to redisplay the
;; cursor.
(add-hook! read-event-hook (lambda (x)
                             (set! (!last-tick current-window) -1)))

(define-method (initialize (self <nomad-gtk-window>) args)
  (next-method)
  (nomad-app-set-style (!mode-line self) "textview text { background-color: #BFBFBF; color: black; }")
  (let ((buffer (window-buffer self))
        (box    (make <gtk-vbox> #:spacing 0)))
    (set! (!last-buffer self) (window-buffer self))
    (set! (buffer-modified? buffer) #t)
    (gtk-box-pack-start box (!container self) #t #t 0)
    (gtk-box-pack-end box (make <widget-border>) #f #f 0)
    (gtk-box-pack-end box (!mode-line self) #f #f 0)
    (gtk-box-pack-end box (make <widget-border>) #f #f 0)
    (gtk-container-add (!widget self) box)
    (cond
       ((is-a? buffer <text-buffer>)
        (when (not (user-data self))
          (dimfi "Create control")
          (set! (user-data self) (make <widget-text-view>))))
       (else
        (error (format #t "user-data for class-of: ~a Not implemented"
                       (class-of buffer)))))
    (g-timeout-add 50 (lambda _
                        (when (and (not (eq? (window-buffer self) (current-buffer)))
                                   (not emacsy-display-minibuffer?))
                          (set! (user-data self) #f)
                          (set! (window-buffer self) (current-buffer)))
                        (redisplay self)
                        #t))))

(define-method (needs-redisplay? (window <nomad-gtk-window>))
  (let* ((buffer (window-buffer window))
         (buffer-tick (buffer-modified-tick buffer))
         (window-tick (!last-tick window)))
    (not (= buffer-tick window-tick))))

(define-method (redisplayed! (window <nomad-gtk-window>))
  (let* ((buffer (window-buffer window))
         (buffer-tick (buffer-modified-tick buffer)))
    (set! (!last-tick window) buffer-tick)))

(define-method (container-child (self <nomad-gtk-window>))
  (let* ((container (!container self))
         (widget    (car (gtk-container-get-children container))))
    widget))

(define-method (child? (self <nomad-gtk-window>))
  (not (container-empty? (!container self))))

(define-method (set-child (self <nomad-gtk-window>))
  (when (child? self)
    (remove-child self))
  (let* ((container (!container self))
         (widget    (user-data self)))
    (gtk-box-pack-start container widget #t #t 0)
    (gtk-widget-show-all container)
    (gtk-widget-grab-focus widget)))

(define-method (remove-child (self <nomad-gtk-window>))
  (let* ((container (!container self))
         (widget    (car (gtk-container-get-children container))))
    (gtk-container-remove container widget)))

(define-method (redisplay (self <nomad-gtk-window>))
  (let* ((buffer    (window-buffer self)))

    (when (not (eq? (!last-buffer self) buffer))
      (dimfi "Remove control" last-buffer)
      (set! (!last-tick self) -1)
      (set! (user-data self) #f))

    (when (needs-redisplay? self)
    (dimfi "Redisplay" (user-data self) buffer)
      (cond
       ((is-a? buffer <text-buffer>)
        (when (not (user-data self))
          (dimfi "Create control")
          (set! (user-data self) (make <widget-text-view>)))
        (set-source-text! (user-data self) (buffer:buffer-string buffer))
        (set-source-point! (user-data self) (buffer:point buffer)))
       ((is-a? buffer <gtk-widget-buffer>)
        (when (not (user-data self))
          (set! (user-data self) buffer)))
       (else
        (error (format #t "user-data for class-of: ~a Not implemented"
                       (class-of buffer))))))
    (redisplayed! self)
    ;; Make sure the window contains the control
    (set-child self)
    (set! (!last-buffer self) buffer)))


