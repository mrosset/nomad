;; doc --- Self-documentation for Nomad

;; Copyright (C) 2019  Amar Singh<nly@disroot.org>

;; This file is part of Nomad.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nomad doc)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (nomad util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (texinfo html)
  #:use-module (texinfo plain-text)
  #:use-module (texinfo reflection)
  #:use-module (texinfo)
  #:export (doc-names
            doc-object
            module-filename
            doc-get
            doc-loc
            nomad-find-doc
            find-doc
            <doc>
            !module
            !name
            !type
            !object
            !docstring
            filename
            module-uri
            module->string
            doc->shtml
            doc->plain-text))

(define module-filename (@@ (ice-9 session) module-filename))

(define-class <doc> ()
  (module    #:accessor     !module
             #:init-keyword #:module)
  (name      #:accessor     !name
             #:init-keyword #:name)
  (type      #:accessor     !type
             #:init-keyword #:type)
  (object    #:accessor     !object
             #:init-keyword #:object)
  (docstring #:accessor     !docstring
             #:init-keyword #:doctring))

(define-method (module->string (doc <doc>))
  (format #f "~a" (module-name (!module doc))))

(define-method (doc->stexi (doc <doc>))
  "Returns the @var<doc> docstring as a stexi expression."
  (texi-fragment->stexi (or (!docstring doc)
                            "No Documentation string.")))

(define-method (doc->stexi (doc <doc>) (full <boolean>))
  "Returns the @var<doc> docstring as a stexi expression. If @var{full} is #t
then it returns the @var{doc}'s object stexi documentation and not just the
docstring as a stexi fragment."
  (if full
      (object-stexi-documentation (!object doc) (!name doc))
      (doc->stexi doc)))

(define-method (doc->plain-text (doc <doc>))
  "Returns the @var<doc> stexi docstring as plain text this is more complete
then the !docstring method"
  (stexi->plain-text (object-stexi-documentation (!object doc) (!name doc))))

(define-method (doc->shtml (doc <doc>))
  (stexi->shtml (doc->stexi doc)))

(define-method (doc->shtml (doc <doc>) (full <boolean>))
  (stexi->shtml (doc->stexi doc full)))

(define-method (filename (doc <doc>))
  (let ((file (module-filename (module-name (!module doc)))))
    (unless file
      (throw 'unknown-module-file (!name doc)))
    file))

(define-method (module-uri (doc <doc>))
  (catch 'unknown-module-file
    (lambda _
      (string-append "nomad://file" (filename doc)))
    (lambda _
      "nomad:not-found")))

(define (nomad-load-path)
  (dirname (dirname (current-filename))))

;; (define (module-filename module)
;;   (string-append (nomad-load-path) //
;;                  (string-join (map symbol->string module) //) ".scm"))

(define (doc-loc file sym)
  "Return the source properties for @var{symbol} definition in @var{file}"
  ;; let ((file (module-filename (module-name (!module (find-doc sym))))))
  (call-with-input-file file
    (lambda (port)
      (let loop ()
        (match (read port)
          ((? eof-object?)
           #f)
          ;; binding
          (('define binding value)
           (if (or (eq? binding sym)
                   (and (list? binding)
                        (eq? (car binding) sym)))
               (source-properties value)
               (loop)))
          ;; class
          (('define-class class (supers) slot)
           (if (eq? class sym)
               (source-properties slot)
               (loop)))
          ;; optional
          (('define* proc value)
           (if (and (list? proc)
                    (eq? (car proc) sym))
               (source-properties proc)
               (loop)))
          (_
           (loop)))))))

(define (doc-names module kind depth)
  "In MODULE, find available objects of KIND, recurse until DEPTH."
  (emacsy-collect-kind (resolve-module module) kind depth))

;;
(define (doc-object module name)
  "Resolve in MODULE, an object NAME."
  (module-ref (resolve-module module) name))

;;
(define (doc-get module name)
  "Get the documentation for an object, looking in MODULE, for NAME."
  (object-documentation (doc-object module name)))

(define (nomad-find-doc sym)
  "Finds the doc-string in known nomad modules"
  (or (false-if-exception (doc-get '(nomad nomad) sym))
      (false-if-exception (doc-get '(emacsy emacsy) sym))
      (false-if-exception (doc-get '(guile-user) sym))))

;; Portions of this procedures are derived from guile (ice-9 session)
;; help-doc. Though it is not an exact copy we should add a copyright for
;; this. guile is copyright of Free Software Foundation, Inc under GPL Version 3
(define (find-doc sym)
  "Finds documentation details for @var{sym} and returns a @var{<doc>} or a
list of @var{doc}.  If no symbols are found it returns #f."
  (let ((found (apropos-fold
                (lambda (module name object data)
                  (cons (make <doc>
                          #:module module
                          #:name (symbol->string name)
                          #:type (cond
                                  ((memq name (commands global-cmdset))
                                   "interactive command")
                                  (else
                                   (class-name (class-of object))))
                          #:object object
                          #:doctring (object-documentation object))
                        data))
                '()
                (simple-format
                 #f "^~A$"
                 (regexp-quote (symbol->string sym)))
                apropos-fold-exported)))
    (if (> (length found) 0)
        (car found)
        #f)))
