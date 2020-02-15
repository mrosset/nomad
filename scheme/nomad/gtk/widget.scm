;; widgets.scm
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

(define-module (nomad gtk widget)
  #:use-module (srfi srfi-26)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<widget-source-view>
            <widget-border>
            <widget-mini-popup>
            <widget-text-view>
            <widget-window>
            !grid
            set-source-text!
            set-source-point!
            show-all
            container
            container-child
            container-replace
            container-empty?))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (gi-import "Gdk")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("Gtk" . "CssProvider")
              ("Gtk" . "StyleContext")
              ("Gtk" . "VBox")
              ("Gtk" . "DrawingArea")
              ("Gtk" . "ScrolledWindow")
              ("Gtk" . "Grid")
              ("Gtk" . "VSeparator")
              ("GtkSource" . "View")
              ("GtkSource" . "Buffer")
              ("GtkSource" . "Language")
              ("GtkSource" . "StyleScheme")
              ("GtkSource" . "StyleSchemeManager")
              ("GtkSource" . "LanguageManager")))
  (gi-import "Nomad"))



;; Widgets that have an associated buffer
(define-class <widget-buffer> ()
  (buffer #:accessor !buffer #:init-keyword #:buffer #:init-value #f))


;; <widget-source-view> provides additional construction and initialization of
;; <gtk-source-view> specialized for nomads text views FIXME: this is
;; redundant due to <widget-text-view> but this is extra special due to
;; modeline and minibuffer switch this to use <nomad-text-view>
(define-class <widget-source-view> (<widget-buffer> <gtk-source-view>)
  (theme #:accessor !theme #:init-keyword #:theme #:init-value "classic")
  (thunk  #:accessor !thunk
          #:init-keyword #:thunk
          #:init-value (lambda _ "no text thunk defined.")))

(define-method (initialize (self <widget-source-view>) args)
  (next-method)
  ;; Setup controls
  ;;
  ;; Since emacsy does all of the editing. We can use
  ;; overwrite mode which provides a block cursor.
  (set-source-theme! self (!theme self))
  (set-source-language! self "scheme")

  ;; https://developer.gnome.org/gtksourceview/stable/GtkSourceView.html
  (nomad-app-set-style self "textview { font-size: 10pt; }")

  (gtk-text-view-set-overwrite self #t)

  (g-timeout-add 50 (lambda _
                      (unless emacsy-display-minibuffer?
                        (emacsy-tick))
                      (redisplay self)
                      #t)))

(define-method (redisplay (self <widget-source-view>))
  (set-source-text! self ((!thunk self)))
  (when (!buffer self)
    (set-source-point! self (buffer:point (!buffer self)))))



(define-class <widget-text-view> (<widget-buffer> <gtk-source-view>)
  (theme  #:accessor !theme  #:init-keyword #:theme  #:init-value "classic")
  (styles #:accessor !styles #:init-keywork #:styles #:init-value '()))

(define-method (initialize (self <widget-text-view>) args)
  (next-method)
  ;; Setup controls
  ;;
  ;; Since emacsy does all of the editing. We can use
  ;; overwrite mode which provides a block cursor.
  (gtk-text-view-set-overwrite self #t)
  (set-source-theme! self (!theme self))
  (set-source-language! self "scheme")

  ;; https://developer.gnome.org/gtksourceview/stable/GtkSourceView.html
  ;;                      "textview { font-family: Monospace; font-size: 10pt;}")
  (map (cut nomad-app-set-style self <>) (!styles self))
  (let ((buffer (!buffer self)))
  (when buffer
    (g-timeout-add 50 (lambda _
                        (set-source-text! self (buffer:buffer-string buffer))
                        (set-source-point! self (buffer:point buffer))
                        #t)))))



(define-class <widget-border> (<gtk-drawing-area>))

(define-method (initialize (self <widget-border>) args)
  (next-method)
  (gtk-widget-set-size-request self -1 1)
  (connect self 'draw nomad-draw-border))



(define-class <widget-window> (<widget-buffer> <gtk-vbox>)
  (container #:accessor     container
             #:init-form    (make <gtk-vbox> #:spacing 0))
  (user-data #:accessor     user-data
             #:init-value   #f)
  (window    #:accessor     !window
             #:init-keyword #:window)
  (mode-line #:accessor     !mode-line
             #:init-form    (make <widget-source-view>
                            #:top-margin 1
                            #:bottom-margin 1
                            #:thunk emacsy-mode-line)))

(define-method (initialize (self <widget-window>) args)
  (next-method)
  (nomad-app-set-style (!mode-line self) "textview text { background-color: #BFBFBF; color: black; }")
  (set! (!thunk (!mode-line self))
        (lambda _
          (with-buffer (!buffer self)
            (emacsy-mode-line))))
  (let ((window (!window self))
        (buffer (!buffer self)))
    (gtk-box-pack-start self (container self) #t #t 0)
    (gtk-box-pack-start self (make <widget-border>) #f #f 0)
    (gtk-box-pack-start self (!mode-line self) #f #f 0)
    (gtk-box-pack-start self (make <widget-border>) #f #f 0)
    (set! (user-data window) self)))



(define-method (show-all (self <gtk-widget>))
  (gtk-widget-show-all self))

;; These methods work on base GTK classes.
(define-method (set-source-theme! (self <gtk-source-view>) text)
  (let* ((buf     (gtk-text-view-get-buffer self))
         (manager (make <gtk-source-style-scheme-manager>))
         (style   (gtk-source-style-scheme-manager-get-scheme manager text)))
    (gtk-source-buffer-set-style-scheme buf style)))


(define-method (set-source-language! (self <gtk-source-view>) text)
  (let* ((buf     (gtk-text-view-get-buffer self))
         (manager (make <gtk-source-language-manager>))
         (lang    (gtk-source-language-manager-get-language manager text)))
    (gtk-source-buffer-set-language buf lang)))

(define-method (set-source-text! (self <gtk-source-view>) text)
  "Sets source @var{view} text buffer to @var{text}"
  (let ((buf (gtk-text-view-get-buffer self)))
    (gtk-text-buffer-set-text buf text -1)))

(define-method (set-source-point! (self <gtk-source-view>) pos)
  "Sets source @var{view} cursor point to @var{pos}"
  (let* ((buf  (gtk-text-view-get-buffer self))
         (iter (gtk-text-buffer-get-start-iter buf)))
    (gtk-text-buffer-get-start-iter buf)
    (gtk-text-iter-forward-chars iter
                                (- pos 1))
    (gtk-text-buffer-place-cursor buf iter)))

(define-method (container-child (self <gtk-container>))
  (car (gtk-container-get-children self)))

(define-method (container-replace (self <gtk-container>) widget)
  (when (not (container-empty? self))
    (gtk-container-remove self (container-child self)))
  (gtk-box-pack-start self widget #t #t 0))

(define-method (container-empty? (self <gtk-container>))
  (= (length (gtk-container-get-children self)) 0))
