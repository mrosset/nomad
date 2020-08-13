;; views.scm
;; Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

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

(define-module (nomad views)
  #:use-module (ice-9 match)
  #:use-module (nomad application)
  #:use-module (emacsy emacsy)
  #:use-module (nomad doc)
  #:use-module (nomad buffer)
  #:use-module (nomad util)
  #:use-module (nomad html)
  #:use-module (oop goops)
  #:export (restful-view
            %nomad-restful-views))

(define style-sheet "
table, th, td {
 border: 1px solid black;
}

td {
width: 33.3%;
}
")

(define (restful-ref views path)
  "Returns a sxml @var{path} from a alist @var{views}"
  (let ((view (assoc-ref views path)))
    (if view
        view
        `("Error: View not found" . , 404-view ))))

(define (restful-view path)
  (let* ((view (restful-ref %nomad-restful-views path))
         (title (car view))
         (thunk (cdr view)))
    (catch #t
      (lambda _
        (thunk title))
      (lambda (key . vals)
        (co-message "Error: key: ~a Value: ~a" key vals)))))

(define-syntax define-view
  (syntax-rules ()
    ((_ (proc) thunk)
     (define-public (proc title)
       "proc returns a html string"
       (sxml->html-string
        `(html
          (head
           (style ,style-sheet)
           (title ,title))
          (body (@ (style "background-color: #F0EAD6;" )) ,thunk)))))))

(define entries (@@ (emacsy keymap) entries))

(define (command->proc-name command)
  (let* ((name       (command-name command))
         (trampoline (if (symbol? name)
                         (symbol->string (command-name command))
                         (begin (dimfi name) "failed-trampoline")))
         (proc       (string-drop-right trampoline 11)))
      proc))

(use-modules (g-golf))

(define (keymap->table keymap)
  `(table (th "Key") (th "interactive command") (th "description")
          ,(map (lambda (value)
                  `(tr (td (@ (style "text-align:center;")) ,(car value))
                       (td ,(if (command? (cdr value))
                                (command->proc-name (cdr value))
                                (class-name (class-of (cdr value)))))
                       (td ,(catch 'misc-error
                              (lambda _
                                (if (command? (cdr value))
                                    (doc-get '(nomad nomad) (string->symbol (command->proc-name (cdr value))))
                                    "keymap"))
                              (lambda _
                                "unresolved symbol")))))
                (hash-map->list cons (entries keymap)))))

(define-view (root-view)
  (begin
    (rename-buffer (current-buffer) "Welcome")
    `((h3 (@ (align "center")) "Welcome to " (a (@ (href "https://www.nongnu.org/nomad/index.html")) "Nomad"))
      (p "Nomad is a Emacs-like web browser (and more) that consists of a modular feature-set, fully programmable in Guile Scheme.")
      ;; (h3 "Getting Started")
      (h4 "Web View Keymap")
      ,(keymap->table (@ (nomad web) %web-mode-map))
      ;; (h5 "Global Keymap")
      ;; ,(keymap->table (@ (nomad ibuffer) ibuffer-map))
      )))

(define-view (404-view)
  (begin
    '(h1 "404 view not found ")))

(define-view (info-view)
  `((h2 (@ (align "center")) "Info")))

(define %nomad-restful-views `(("" . ("Welcome" . ,root-view))))

;; ((@ (nomad web-mode) reload))
