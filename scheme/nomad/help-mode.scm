;; help-mode.scm
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

(define-module (nomad help-mode)
  #:use-module (ice-9 readline)
  #:use-module (emacsy emacsy)
  #:use-module (oop goops)
  #:use-module (nomad doc)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad html)
  #:use-module (nomad views)
  #:use-module (nomad platform)
  #:export (<help-buffer>
            %help-mode-map
            help-mode))

(define help-mode (make <mode> #:mode-name "Help"))

(define %help-mode-map (make-keymap))

(define-class <help-buffer> (<web-buffer>)
  (keymap #:accessor local-keymap
          #:init-keyword #:keymap
          #:init-value %help-mode-map)
  (name   #:init-value "*Help*")
  (uri    #:accessor     buffer-uri
          #:init-keyword #:uri
          #:init-value  "nomad:unknown")
  (modes  #:accessor buffer-modes
          #:init-value (list help-mode web-mode))
  (view   #:accessor !view
          #:init-keyword #:view
          #:init-form (404-view "Help not found.")))

(define-method (initialize (buffer <help-buffer>) args)
  (next-method)
  (when (!view buffer)
    (load-html buffer (!view buffer) (buffer-uri buffer))))

(define-view (describe-view symbol)
  "returns a HTML string describing @var{symbol}"
  (let* ((str   (symbol->string symbol))
         (ref   (eval symbol (current-module)))
         (class (if (memq symbol (commands global-cmdset))
                    "interactive command"
                    (class-name (class-of ref)))))
    `((p ,(format #f "~a is a ~a in FIXME: location" str class))
      (p ,ref)
      (p ,(doc->shtml symbol)))))

(define-interactive (nomad-help)
  (make-buffer <help-buffer>))

(define-interactive (describe-function
                     #:optional (function (string->symbol (completing-read "Describe Function: " apropos-completion-function))))
  "Display the documentation of @var{function} a symbol."
  (make-buffer <help-buffer>
               #:uri "nomad:describe"
               #:view (describe-view "Describe Function" function)))

;; Global key bindings.
(define-key global-map (kbd "C-h f") 'describe-function)

(define-key %help-mode-map (kbd "q") 'kill-buffer)
