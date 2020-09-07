;; web.scm
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

(define-module (nomad gtk web)
  #:use-module (ice-9 format)
  #:use-module (nomad gtk gi)
  #:use-module (nomad menu)
  #:use-module (nomad web)
  #:use-module (nomad terminal)
  #:use-module (nomad text)
  #:use-module (nomad gtk widget)
  #:use-module (nomad gtk window)
  #:use-module (emacsy emacsy)
  #:use-module (emacsy window)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

(save-module-excursion
 (lambda _
   (set-current-module (resolve-module '(nomad web)))
   (g-export widget-is-loading?
             emacsy-mode-line
             widget-uri
             buffer-title
             buffer-load-uri
             load-html
             buffer-back
             buffer-forward
             buffer-hints
             hints-finish
             buffer-scroll-up
             buffer-scroll-down
             buffer-scroll-top
             buffer-page-up
             buffer-page-down
             buffer-scroll-bottom
             buffer-reload
             search-forward
             search-finish)

   (define-method (initialize (buffer <web-buffer>) arg)
     (next-method)
     (add-hook! (buffer-enter-hook buffer)
                (lambda _
                  (run-hook %menu-bar-hook)))
     (set! (buffer-widget buffer)
           (make <widget-web-view> #:buffer buffer)))

   (define-method (buffer-title (buffer <web-buffer>))
     (if (!is-loading (buffer-widget buffer))
         "loading..."
         (get-title (buffer-widget buffer))))

   (define-method (widget-is-loading? (buffer <web-buffer>))
     (!is-loading (buffer-widget buffer)))

   (define-method (widget-uri (buffer <web-buffer>))
     (get-uri (buffer-widget buffer)))

   (define-method (emacsy-mode-line (buffer <web-buffer>))
     (format #f "~a~/~a%"
             (next-method)
             (buffer-progress buffer)))

   (define-method (emacsy-mode-line (buffer <terminal>))
     (let ((vte (buffer-widget buffer)))
       (format #f "~a~/~a"
               (next-method)
               (get-window-title vte))))

   (define-method (load-html (buffer <web-buffer>)
                             (html <string>)
                             uri)
     (load-html (buffer-widget buffer) html uri))

   (define-method (buffer-load-uri (buffer <web-buffer>) uri)
     (webkit-web-view-load-uri (buffer-widget buffer)
                               uri))

   (define-method (buffer-forward (buffer <web-buffer>))
     (go-forward (buffer-widget buffer)))

   (define-method (buffer-back (buffer <web-buffer>))
     (go-back (buffer-widget buffer)))

   (define-method (buffer-reload (buffer <web-buffer>))
     (webkit-web-view-reload (buffer-widget buffer)))

   ;; (define-syntax call-javascript
   ;;   (lambda (x)
   ;;     (syntax-case x ()
   ;;       ((_ js)
   ;;        #`(run-javascript #,(datum->syntax x '(buffer-widget buffer)) js #f #f #f)))))

   (define-method (buffer-scroll-up (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollBy(0, -25);" #f #f #f))

   (define-method (buffer-scroll-down (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollBy(0, 25);" #f #f #f))

   (define-method (buffer-scroll-top (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollTo(0, 0);" #f #f #f))

   (define-method (buffer-scroll-bottom (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollTo(0,document.body.scrollHeight);" #f #f #f))

   (define-method (buffer-page-up (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollBy(0, -window.innerHeight + 25);" #f #f #f))

   (define-method (buffer-page-down (buffer <web-buffer>))
     (run-javascript (buffer-widget buffer)
                     "window.scrollBy(0,window.innerHeight - 25);" #f #f #f))

   (define-method (hints-finish (buffer <web-buffer>))
     (nomad-app-send-message (buffer-widget buffer)
                             (make <webkit-user-message> #:name "hints-finish")))

   (define-method (buffer-hints (buffer <web-buffer>))
     (nomad-app-send-message (buffer-widget buffer)
                             (make <webkit-user-message> #:name "show-hints")))

   (define-method (search-forward (buffer <web-buffer>))
     (let ((controller (get-find-controller (buffer-widget buffer))))
       (search controller (current-search buffer) 1 255)))

   (define-method (search-finish (buffer <web-buffer>))
     (set! (current-search buffer) #f)
     (let ((controller (get-find-controller (buffer-widget buffer))))
       (search-finish controller)))))

(save-module-excursion
 (lambda _
   (set-current-module (resolve-module '(nomad text)))
   (g-export buffer-proxy-set!)
   (define-method (initialize (buffer <widget-buffer>) args)
     (next-method)
     (add-hook! (buffer-kill-hook buffer)
                (lambda _
                  (destroy (buffer-widget buffer))
                  (prev-buffer)
                  (set! (window-buffer current-window) (current-buffer)))))

   (define-method (buffer-proxy-set! (buffer <widget-buffer>) proxy)
     (let ((widget (buffer-widget buffer)))
       (if widget
           (set-network-proxy-settings
            widget
            'custom
            proxy))))))

(define-interactive (webkit-version)
  (message "~a.~a.~a"
              (webkit-get-major-version)
              (webkit-get-minor-version)
              (webkit-get-micro-version)))
