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
  #:use-module (nomad gtk gi)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad web)
  #:use-module (nomad widget)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk menu)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (<widget-window>))

(g-export widget-container
          window-widget
          window-buffer)

(define-class <widget-window> (<window>)
  (last-tick #:accessor last-tick #:init-value -1))

(define-method (window-widget (window <widget-window>))
  (!widget (user-data window)))

(define-method (widget-container (window <widget-window>))
  (!container (user-data window)))

(define-method (widget-buffer (window <widget-window>))
  (!buffer (user-data window)))

(define-public (window-config-change window)
   (container-replace (!container (current-frame)) (instantiate-window root-window)))

(add-hook! window-configuration-change-hook window-config-change)

(define (make-gtk-window list vertical)
  (let ((box (if vertical
                 (make <gtk-vbox>)
                 (make <gtk-hbox>))))
    (for-each (lambda (widget)
                (when  (!parent widget)
                  (gtk-box-pack-start box widget #t #t 0))
                (gtk-box-pack-start box widget #t #t 0) )
              list)
    (gtk-widget-show-all box)
    box))

(define-method (instantiate-window (window <widget-window>))
  (make <window-container> #:window window #:buffer (window-buffer window)))

(define-method (instantiate-window (window <internal-window>))
  (make-gtk-window (map instantiate-window (window-children window))
                   (eq? (orientation window) 'vertical)))

(define-method (redisplay (window <internal-window>))
  (for-each redisplay (window-children window)))

(define-method (redisplay (window <window>))
  (when (is-a? (window-buffer window) <text-buffer>)
   (catch 'no-such-local-variable
     (lambda _
       (local-var 'widget))
     (lambda (key . vals)
       (set! (local-var 'widget) #f)))))

(define-method (redisplay (window <widget-window>))
  (next-method)
  (let* ((buffer    (window-buffer window))
         (view      (user-data window))
         (container (widget-container window))
         (widget    (cond ((is-a? buffer <widget-buffer>)
                           (buffer-widget buffer))
                          ((is-a? buffer <text-buffer>)
                           (local-var 'widget))
                          (else (error "Buffer not implimented")))))
    (unless widget
      (cond
       ((is-a? buffer <widget-buffer>)
        (set! widget (make-buffer-widget buffer))
        (set! (buffer-widget buffer) widget))
       ((is-a? buffer <text-buffer>)
        (set! widget (make <widget-text-view> #:buffer buffer)))
       (else (error "Buffer not implimented")))
      (set! (!buffer (user-data window)) buffer)
      (set! (local-var 'widget) widget)
      (container-replace container widget)
      (run-hook %thunk-view-hook))

    ;; Text buffers are not derived from <widget-buffer> so we need to switch
    ;; to the right menu-bar if needed here
    (when (and (is-a? buffer <text-buffer>)
               (not (eq? (get-titlebar (current-frame))
                         (menu-bar buffer))))
      (set-titlebar (current-frame) (menu-bar buffer)))

    (when (not (eq? buffer (!buffer (user-data window))))
      (set! (!buffer (user-data window)) buffer)
      (container-replace container widget)
      (run-hook %thunk-view-hook))))

(define-method (needs-redisplay? (widget <widget-text-view>) (window <widget-window>))
         (let* ((buffer (window-buffer window))
                (buffer-tick (buffer-modified-tick buffer))
                (window-tick (last-tick window)))
           (or (not (= buffer-tick window-tick))
                     (!thunk widget))))

(define-method (redisplayed! (window <widget-window>))
  (let* ((buffer (window-buffer window))
         (buffer-tick (buffer-modified-tick buffer)))
    (set! (last-tick window) buffer-tick)))
