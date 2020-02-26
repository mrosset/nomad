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
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (nomad text)
  #:use-module (nomad web)
  #:use-module (nomad ibuffer)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:export (<widget-web-view>
            <widget-border>
            <widget-mini-popup>
            <widget-text-view>
            <widget-thunk-view>
            redisplay
            <window-container>
            make-buffer-widget
            !grid
            !thunk
            !mode-line
            set-text!
            get-text
            set-point!
            show-all
            grab-focus
            !container
            container-child
            container-replace
            container-empty?))

(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (gi-import "Gdk")
  (for-each (lambda (x)
              (gi-import-by-name  (car x) (cdr x)))
            '(("WebKit2" . "WebView")
              ("Gtk" . "CssProvider")
              ("Gtk" . "Clipboard")
              ("Gtk" . "StyleContext")
              ("Gtk" . "VBox")
              ("Gtk" . "Label")
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
(define-class <widget-with-buffer> ()
  (buffer #:accessor !buffer #:init-keyword #:buffer #:init-value #f))



(define-class <widget-text-view> (<widget-with-buffer>
                                  <gtk-source-view>)
  (last-tick  #:accessor     last-tick
              #:init-value   -2)
  (last-pos   #:accessor     last-pos
              #:init-value   -1)
  (theme      #:accessor     !theme
              #:init-keyword #:theme
              #:init-value   "classic")
  (lang       #:accessor     !lang
              #:init-keyword #:language
              #:init-value   "scheme")
  (styles     #:accessor     !styles
              #:init-keyword #:styles
              #:init-form   '("textview { font-family: \"DejaVu Sans Mono\"; font-weight: normal; font-stretch: normal}")))

(define-method (initialize (self <widget-text-view>) args)
  (next-method)
  ;; Setup controls
  ;;
  ;; Since emacsy does all of the editing. We can use
  ;; overwrite mode which provides a block cursor.
  (gtk-text-view-set-overwrite self #t)

  ;; Set theme and language
  (set-theme! self (!theme self))
  (set-language! self (!lang self))

  (connect self 'paste-clipboard
           (lambda _
             (let* ((clipboard (gtk-clipboard-get-default (gdk-display-get-default)))
                    (input     (gtk-clipboard-wait-for-text clipboard)))
               (insert input))))

  ;; https://developer.gnome.org/gtksourceview/stable/GtkSourceView.html
  (for-each (lambda (style)
              (nomad-app-set-style self style))
            (!styles self))

  (when (!buffer self)
    (g-timeout-add 50 (lambda _
                        (redisplay self)
                        #t))))

(define-method (redisplay (self <widget-text-view>))
  (let ((buffer (!buffer self)))
    (unless  (eq? (buffer-modified-tick buffer) (last-tick self))
      (set-text! self (buffer:buffer-string buffer))
      (set! (last-tick self) (buffer-modified-tick buffer))
      (set! (last-pos self) -2))
    (unless (and (eq? (buffer:point buffer) (last-pos self)))
      (set-point! self (buffer:point buffer))
      (set! (last-pos self) (buffer:point buffer)))))



(define-public %thunk-view-hook (make-hook 0))

(define-class <widget-thunk-view> (<widget-text-view>)
  (thunk #:accessor     !thunk
         #:init-keyword #:thunk))


(define-method (initialize (self <widget-thunk-view>) args)
  (next-method)
  (add-hook! %thunk-view-hook
             (lambda _
               (set-text! self ((!thunk self)))
               (when (!buffer self)
                 (set-point! self (buffer:point (!buffer self)))))))



(define-class <widget-border> (<gtk-drawing-area>))

(define-method (initialize (self <widget-border>) args)
  (next-method)
  (gtk-widget-set-size-request self -1 1)
  (connect self 'draw nomad-draw-border))



(define-class <window-container> (<widget-with-buffer> <gtk-vbox>)
  (container #:accessor     !container
             #:init-form    (make <gtk-scrolled-window>))
  (window    #:accessor     !window
             #:init-keyword #:window)
  (mode-line #:accessor     !mode-line
             #:init-form    (make <widget-thunk-view>
                              #:styles '("textview text { font-family: \"DejaVu Sans Mono\"; background-color: #BFBFBF; color: black; }")
                              #:top-margin 1
                              #:bottom-margin 1
                              #:cursor-visible #f
                              #:thunk emacsy-mode-line)))

(define-method (initialize (self <window-container>) args)
  (next-method)

  (let* ((mode-line (!mode-line self))
         (buf (gtk-text-view-get-buffer mode-line)))
    (gtk-source-buffer-set-highlight-syntax buf #f)
    (gtk-source-buffer-set-highlight-matching-brackets buf #f))

  (set! (!thunk (!mode-line self))
        (lambda _
          (with-buffer (!buffer self)
            (emacsy-mode-line))))

  (let ((window (!window self)))
    (gtk-box-pack-start self (!container self) #t #t 0)
    (gtk-box-pack-start self (make <widget-border>) #f #f 0)
    (gtk-box-pack-start self (!mode-line self) #f #f 0)
    (gtk-box-pack-start self (make <widget-border>) #f #f 0)
    (set! (user-data window) self)))



(define-class <widget-web-view> (<widget-with-buffer>
                                 <webkit-web-view>))

(define-method (widget-load-uri (view <widget-web-view>) uri)
  (webkit-web-view-load-uri view uri)
  (show-all view))

(define-method (initialize (view <widget-web-view>) args)
  (next-method)
  (connect view 'load-changed
           (lambda _
             (let ((buffer (!buffer view)))
               (set! (buffer-title buffer)
                     (!title view))
               (set! (buffer-uri buffer)
                     (!uri view))
               (set! (buffer-progress buffer)
                     (inexact->exact
                      (round (* 100 (!estimated-load-progress view)))))
               (run-hook %thunk-view-hook))
             #t))
  (widget-load-uri view (buffer-uri (!buffer view))))

(define-method (redisplay (view <widget-web-view>))
  #t)



(define-method (make-buffer-widget (buffer <web-buffer>))
  (make <widget-web-view> #:buffer buffer))

(define-method (make-buffer-widget (buffer <ibuffer>))
  (let* ((view (make <widget-text-view>
                #:buffer buffer
                #:highlight-current-line #t))
         (buf (gtk-text-view-get-buffer view)))
    (gtk-source-buffer-set-highlight-syntax buf #f)
    (gtk-source-buffer-set-highlight-matching-brackets buf #f)
    view))

;; Base GTK methods.
(define-method (show-all (widget <gtk-widget>))
  (gtk-widget-show-all widget))

(define-method (grab-focus (widget <gtk-widget>))
  (gtk-widget-grab-focus widget))

(define-method (get-text (widget <gtk-text-view>))
  (let* ((buf   (gtk-text-view-get-buffer widget))
         (start (gtk-text-buffer-get-start-iter buf))
         (end   (gtk-text-buffer-get-end-iter buf)))
    (gtk-text-buffer-get-text buf start end #t)))

(define-method (set-theme! (self <gtk-source-view>) text)
  (let* ((buf     (gtk-text-view-get-buffer self))
         (manager (make <gtk-source-style-scheme-manager>))
         (style   (gtk-source-style-scheme-manager-get-scheme manager text)))
    (gtk-source-buffer-set-style-scheme buf style)))


(define-method (set-language! (self <gtk-source-view>) text)
  (let* ((buf     (gtk-text-view-get-buffer self))
         (manager (make <gtk-source-language-manager>))
         (lang    (gtk-source-language-manager-get-language manager text)))
    (gtk-source-buffer-set-language buf lang)))

(define-method (set-text! (self <gtk-source-view>) text)
  "Sets source @var{view} text buffer to @var{text}"
  (let ((buf (gtk-text-view-get-buffer self)))
    (gtk-text-buffer-set-text buf text -1)))

(define-method (set-point! (self <gtk-source-view>) pos)
  "Sets source @var{view} cursor point to @var{pos}"
  (let* ((buf  (gtk-text-view-get-buffer self))
         (iter (gtk-text-buffer-get-start-iter buf)))
    (gtk-text-buffer-get-start-iter buf)
    (gtk-text-iter-forward-chars iter
                                (- pos 1))
    (gtk-text-buffer-place-cursor buf iter)))

(define-method (container-child (self <gtk-container>))
  (if (container-empty? self)
      #f
      (car (gtk-container-get-children self))))

(define-method (container-replace (self <gtk-container>) widget)
  (when (not (container-empty? self))
    (gtk-container-remove self (container-child self)))
  (gtk-container-add self widget)
  (show-all self)
  (gtk-widget-grab-focus widget))

(define-method (container-empty? (self <gtk-container>))
  (= (length (gtk-container-get-children self)) 0))
