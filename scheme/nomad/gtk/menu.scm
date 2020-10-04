;; menu.scm
;; Copyright (C) 2017-2019 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad gtk menu)
  #:use-module (srfi srfi-1)
  #:use-module (nomad gtk gi)
  #:use-module (g-golf)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (nomad menu)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad ibuffer)
  #:use-module (nomad web-mode)
  #:use-module (nomad widget)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk frame)
  #:use-module (nomad gtk util)
  #:duplicates (merge-generics replace warn-override-core warn first)
  #:export (<widget-web-bar>
            <menu-button>))

(g-export menu-bar
          !entry)

(define* (make-menu-button label #:optional (command #f))
  (let ((button (make <gtk-button>
                  #:relief 'none
                  #:xalign 0
                  #:label label)))
    (when command
      (connect button 'clicked
               (lambda _
                 (command))))
    button))

(define-class <menu-button> (<gtk-menu-button>))

(define-method (initialize (self <menu-button>) args)
  (next-method)
  (let* ((icon    (make <gtk-image>
                   #:icon-name "open-menu"))
         (pop     (make <gtk-popover-menu>))
         (box     (make <gtk-vbox> #:margin 10
                        #:halign 'start))
         (buffers (make-menu-button "Buffers" ibuffer))
         (kill    (make-menu-button "kill-buffer" kill-buffer))
         (m-x     (make-menu-button "M-x"
                                    (lambda _
                                      (emacsy-key-event #\x '(meta))
                                      (emacsy-tick))))
         (about   (make-menu-button "About Nomad"))
         (quit    (make-menu-button "Quit Nomad"
                                    (lambda _ ((colambda _ (kill-nomad)))))))

    ;; Model
    ;; (set-menu-model self (make <main-menu>))
    ;; Properties
    (set-relief self 'none)
    (set-popover self pop)
    (set-image self icon)
    ;; Packing
    (pack-start box m-x #f #f 1)
    (pack-start box buffers #f #f 1)
    (pack-start box kill #f #f 1)
    (pack-start box (make <gtk-vseparator>) #f #f 1)
    (pack-start box about #f #f 2)
    (pack-start box (make <gtk-vseparator>) #f #f 1)
    (pack-start box quit #f #f 1)

    (show-all box)
    (add pop box)))

(define-method (make-icon-button (id <string>))
  (let ((btn (gtk-button-new-from-icon-name
              id
              (enum-ref 'gtk-icon-size 'small-toolbar))))
    btn))

;; Web menu bar
(define-class <widget-web-bar> (<gtk-header-bar>)
  (entry     #:accessor  !entry
             #:init-form (make <widget-entry>
                           #:editable #f
                           #:overwrite-mode #t
                           #:hexpand #t)))

(define-method (initialize (self <widget-web-bar>) args)
  (next-method)
  (let* ((box     (make <gtk-hbox>))
         (context (get-style-context box))
         (back    (make-icon-button "go-previous-symbolic"))
         (forward (make-icon-button "go-next-symbolic"))
         (menu    (make <menu-button>)))

    ;; Style
    ;; (nomad-app-set-style (!entry self) "entry{border: none; box-shadow: none; }")

    ;; Input Box
    (set-custom-title self (!entry self))
    (set-show-close-button self #t)

    ;; Button Box
    (pack-start box back #f #f 0)
    (pack-start box forward #f #f 0)
    (gtk-style-context-add-class context "linked")

    ;; Pack Header
    (pack-start self box)
    (pack-end self menu)

    ;; Signals
    (connect back 'clicked
             (lambda _
               (buffer-back (current-buffer))))
    (connect forward 'clicked
             (lambda _
               (buffer-forward (current-buffer)))))

  (let ((buffer (current-buffer)))
    (add-hook! (!menu-hook buffer)
               (lambda _
                 (let* ((uri  (widget-uri buffer))
                        (icon (cond
                               ((!is-loading (buffer-widget buffer))
                                #f)
                               ((secure? buffer)
                                "channel-secure-symbolic")
                               (else #f))))
                   (set-icon-from-icon-name (!entry self)
                                            'primary
                                            icon)
                   (with-buffer (!buffer (!entry self))
                     (delete-region (point-min) (point-max))
                     (insert (webkit-uri-for-display uri)))))))
  (show-all self)
  (set-visible self (%menu-bar-mode)))

(define-method (menu-bar (buffer <widget-buffer>))
  (!menu (current-frame)))

(define-method (menu-bar (buffer <text-buffer>))
  (!menu (current-frame)))

(define-method (menu-bar (buffer <web-buffer>))
  (unless (!menu buffer)
    (set! (!menu buffer) (make <widget-web-bar>)))
  (!menu buffer))

(set-current-module (resolve-module '(nomad menu)))

(use-modules (nomad gtk frame)
             (nomad gtk widget)
             (nomad gtk menu)
             (nomad widget)
             (nomad web)
             (nomad web-mode))

(define-public %reading-uri? #f)

(define-public (read-query-or-uri initial)
  (catch #t
    (lambda _
      (let ((input (completing-read "Uri or Query: "
                                    '("https://")
                                    #:initial-input initial)))
        (if (valid-uri? input)
            (buffer-load-uri (current-buffer) input)
            (query input (current-buffer)))))
    (lambda (key . args)
      (message "Error: key: ~a Arguments: ~a" key args))))

(define (current-menu-uri)
  "Returns the current @var{<web-buffer>}'s menu-bar URI."
  (buffer:buffer-string (!buffer (!entry (current-menu)))))

(define-interactive (edit-menu-uri)
  "Loads or queries the default search provider using the current
@var{<web-buffer>}. If menu-bar-mode is active it uses the main minibuffer
instead of the URI minibuffer."
  (if (get-visible (current-menu))
      (let ((old-mini minibuffer)
            (buffer   (!buffer (!entry (current-menu)))))
        (dynamic-wind
          (lambda _
            (set! %reading-uri? #t)
            (set! minibuffer buffer))
          (lambda _
            (read-query-or-uri (current-menu-uri)))
          (lambda _
            (set! %reading-uri? #f)
            (set! minibuffer old-mini))))
      (read-query-or-uri (current-menu-uri)))
  #t)

(define-key %web-mode-map (kbd "C-c e") 'edit-menu-uri)

(define-interactive (menu-bar-mode #:optional (enable? (not (%menu-bar-mode))))
  "Toggles the menu-bar-mode on or off."
  (%menu-bar-mode enable?)
  (for-each (lambda (buffer)
              (set-visible (menu-bar buffer) (%menu-bar-mode)))
            (buffer-list))
  (message "~a" (%menu-bar-mode))
  (%menu-bar-mode))
