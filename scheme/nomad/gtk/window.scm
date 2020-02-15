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

(define-public (window-config-change window)
   (container-replace (!root (current-frame)) (instantiate-window root-window)))

;; Redisplay current window on each event read. This is mainly used to redisplay the
;; cursor.
;; (add-hook! read-event-hook (lambda (x)
;;                              ;; (dimfi current-window)
;;                              (window-config-change root-window)))


(add-hook! window-configuration-change-hook window-config-change)



;; (define-class <gtk-internal-window> (<internal-window> <gtk-box>))

;; (define-method (initialize (self <gtk-internal-window>) args)
;;   (next-method))

(define (make-gtk-window list vertical)
  (let ((box (if vertical
                 (make <gtk-vbox>)
                 (make <gtk-hbox>))))
    (for-each (lambda (widget)
                (if (!parent widget)
                    ;; (gtk-widget-reparent widget box)
                    (begin
                      (gtk-container-remove (!parent widget) widget)
                      (gtk-box-pack-start box widget #t #t 0))
                    (gtk-box-pack-start box widget #t #t 0))) list)
    (gtk-widget-show-all box)
    box))

(define-method (instantiate-window (window <window>))
  (make <widget-window> #:window window #:buffer (window-buffer window))
  ;; (let ((box (make <widget-window>)))
  ;;   (gtk-box-pack-start box (make <gtk-label> #:label "Hello") #t #t 0)
  ;;   box)
  ;; (dimfi window)
  ;; window
  ;; (describe window)
  ;; (let ((buffer (window-buffer window)))
  ;;   (create-web-view-window window buffer (is-a? buffer <text-buffer>)))
  )

(define-method (instantiate-window (window <internal-window>))
  (make-gtk-window (map instantiate-window (window-children window))
                                       (eq? (orientation window) 'vertical)))



(define-method (redisplay (window <internal-window>))
  (for-each redisplay (window-children window)))

(define-method (redisplay (window <window>))
  (update (user-data window)))

(define-method (update self)
  (let* ((window   (!window self))
         (buffer   (!buffer self)))
    (when (or (not (user-data self))
              (not (eq? buffer (window-buffer window))))
      (cond
       ((is-a? (window-buffer window) <text-buffer>)
        (set! (user-data self) (make <widget-text-view> #:buffer (window-buffer window))))
       ((is-a? (window-buffer window) <gtk-webview-buffer>)
        (let ((view (make <webkit-web-view>)))
          (webkit-web-view-load-uri view (buffer-uri (window-buffer window)))
          (set! (user-data self) view)))
       (else (error "buffer not implimented")))
      (set! (!buffer self) (window-buffer window))
      (container-replace (container self) (user-data self))
      (gtk-widget-show-all self)
      (gtk-widget-grab-focus (user-data self)))))

;; (define-method (needs-redisplay? (window <nomad-gtk-window>))
;;   (let* ((buffer (window-buffer window))
;;          (buffer-tick (buffer-modified-tick buffer))
;;          (window-tick (!last-tick window)))
;;     (not (= buffer-tick window-tick))))

;; (define-method (redisplayed! (window <nomad-gtk-window>))
;;   (let* ((buffer (window-buffer window))
;;          (buffer-tick (buffer-modified-tick buffer)))
;;     (set! (!last-tick window) buffer-tick)))

;; (define-method (redisplay (self <nomad-gtk-window>))
;;   (let* ((buffer    (window-buffer self)))

;;     (when (not (eq? (!last-buffer self) buffer))
;;       (dimfi "Remove control" last-buffer)
;;       (set! (!last-tick self) -1)
;;       (set! (user-data self) #f))

;;     (when (needs-redisplay? self)
;;     (dimfi "Redisplay" (user-data self) buffer)
;;       (cond
;;        ((is-a? buffer <text-buffer>)
;;         (when (not (user-data self))
;;           (dimfi "Create control")
;;           (set! (user-data self) (make <widget-text-view>)))
;;         (set-source-text! (user-data self) (buffer:buffer-string buffer))
;;         (set-source-point! (user-data self) (buffer:point buffer)))
;;        ((is-a? buffer <gtk-widget-buffer>)
;;         (when (not (user-data self))
;;           (set! (user-data self) buffer)))
;;        (else
;;         (error (format #t "user-data for class-of: ~a Not implemented"
;;                        (class-of buffer))))))
;;     (redisplayed! self)
;;     ;; Make sure the window contains the control
;;     (set-child self)
;;     (set! (!last-buffer self) buffer)))


