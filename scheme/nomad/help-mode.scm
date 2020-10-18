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
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 session)
  #:use-module (nomad doc)
  #:use-module (nomad html)
  #:use-module (nomad util)
  #:use-module (nomad views)
  #:use-module (nomad web)
  #:use-module (oop goops)
  #:use-module (texinfo html)
  #:use-module (texinfo reflection)
  #:export (<help-buffer>
            !view
            %help-mode-map
            help-mode))

(define help-doc (@@ (ice-9 session) help-doc))

(define %help-mode-map (make-keymap))

(define help-mode (make <mode>
                    #:mode-name "Help"
                    #:mode-map %help-mode-map))

(define-class <help-buffer> (<web-buffer>)
  (name   #:init-value "*Help*")
  (uri    #:accessor     buffer-uri
          #:init-keyword #:uri
          #:init-value  "nomad:help")
  (modes  #:accessor buffer-modes
          #:init-form (list help-mode (web-mode)))
  (view   #:accessor !view
          #:init-keyword #:view
          #:init-form (404-view "Help not found.")))

(define-method (initialize (buffer <help-buffer>) args)
  (next-method)
  (when (!view buffer)
    (load-html buffer (!view buffer) (buffer-uri buffer))))

(define-view (describe-object-view symbol)
  "returns a HTML string describing @var{symbol}"
  (let* ((doc (with-output-to-string
                (lambda _
                  (help-doc symbol (format #f "^~A$" symbol))))))
    `(p (@ (style "white-space: pre;")) ,doc))
  ;; (let ((doc (find-doc symbol)))
  ;;   `((p ,(format #f "`~a' is a ~a in the ~a module."
  ;;                 (!name doc)
  ;;                 (!type doc)
  ;;                 (module->string doc)))
  ;;     (p (a (@ (target "_blank") (href ,(module-uri doc))) ,(module->string doc)))
  ;;     (p (@ (style "white-space: pre;")) ,(doc->plain-text doc))))
)

(define (modes->string-names modes)
  "Converts a list of @var{modes} to a string of mode names"
  (string-join (map (lambda (mode)
                      (string-append (mode-name mode) " ")) modes)
               " "))

(define-view (describe-mode-view buffer)
  "Returns a HTML string describing @var{mode}"
  (let* ((modes (buffer-modes buffer))
         (class (class-name (class-of buffer))))
    `((h3 "This is a ",class)
      (p "Enabled modes ",(modes->string-names modes))
      ,(map (lambda (mode)
                       `((h5 "Key bindings for " ,(mode-name mode) " mode")
                         ,(keymap->table (mode-map mode))))
                     modes))))

(define-view (describe-module-view)
  "Returns a HTML string describing @var{module}"
  `(,(stexi->shtml (module-stexi-documentation '(nomad web)))))

(define-interactive (describe-module)
  (make-buffer <help-buffer>
               #:uri "nomad:module"
               #:view (describe-module-view "Describe Module")))

(define-interactive (describe-function
                     #:optional (function (string->symbol (completing-read "Describe Function: " apropos-completion-function))))
  "Display the documentation of @var{function} a symbol."
  (make-buffer <help-buffer>
               #:uri "nomad:describe"
               #:view (describe-object-view "Describe Function" function)))

(define-interactive (describe-mode #:optional (buffer (current-buffer)))
  "Display the documentation for the current mode."
  (make-buffer <help-buffer>
               #:uri "nomad:describe-mode"
               #:view (describe-mode-view "Describe Mode" buffer)))

;; Global key bindings.
(define-key global-map (kbd "C-h f") 'describe-function)
(define-key global-map (kbd "C-h m") 'describe-mode)

(define-key %help-mode-map (kbd "q") 'kill-buffer)
