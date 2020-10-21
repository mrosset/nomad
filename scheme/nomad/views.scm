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
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (nomad application)
  #:use-module (nomad buffer)
  #:use-module (nomad doc)
  #:use-module (nomad html)
  #:use-module (nomad uri)
  #:use-module (nomad util)
  #:use-module (nomad web)
  #:use-module (nomad web-mode)
  #:use-module (oop goops describe)
  #:use-module (oop goops)
  #:use-module (srfi srfi-19)
  #:use-module (system vm coverage)
  #:use-module (system vm vm)
  #:use-module (web uri)
  #:export (restful-view
            define-view
            keymap->table
            %nomad-restful-views))

(define (a key body)
  `(a (@ (target "_blank") (href ,(assoc-ref %links key))) ,body))

(define style-sheet "
body {
background-color: #F0EAD6;
}

table, th, td {
border: 1px solid black;
border-collapse: collapse;
}

th, td {
  padding: 6px;
}

a {
target-name:new;
target-new:tab;
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
    ((_ (proc . args) doc thunk)
     (define-public (proc title . args)
       doc
       (sxml->html-string
        `(html
          (head
           (style ,style-sheet)
           (title "*Help*"))
          (body ,thunk)))))))

(define (command->proc-name command)
  (let* ((name       (command-name command))
         (trampoline (if (symbol? name)
                         (symbol->string (command-name command))
                         "anonymous"))
         (proc       (if (string-suffix? "-trampoline" trampoline)
                         (string-drop-right trampoline 11)
                         trampoline)))
    proc))

(define (key< x y)
  (string< (car x) (car y)))

(define* (entries->row pair #:optional (prefix #f))
  (catch 'self-insert
    (lambda _
      (let* ((key     (car pair))
             (proc    (cdr pair))
             (command (if (command? proc)
                          (let ((name (command->proc-name proc)))
                            (when (string= name "self-insert-command")
                              (throw 'self-insert))
                            name)
                          (class-name (class-of proc))))
             (doc    (catch 'misc-error
                       (lambda _
                         (when (command? proc)
                           (find-doc (string->symbol command))))
                       (lambda _
                         "Unresolved command."))))
        (if (is-a? proc <keymap>)
            (map (lambda (i)
                   (entries->row i key))
                 (sort-list (hash-map->list cons (entries proc)) key<))
            `(tr (td (@ (style "width: 25%; text-align:center;")) ,(if prefix
                                                           (string-append prefix " ")
                                                           "") ,key)
                 (td (@ (style "width: 25%;")) ,command)
                 (td (@ (style "width: 50%;")) ,(or (and doc (doc->shtml doc)) ""))))))
    (lambda _
      `())))

(define (keymap->table keymap)
  `(table (@ (align "center") (width "85%")) (th "Key") (th "Command") (th "Description")
          ,(map entries->row
                (sort-list (hash-map->list cons (entries keymap)) key<))))

(define-view (root-view)
  "Returns the root @url{nomad:} scheme URI view."
  (begin
    (rename-buffer (current-buffer) "Welcome")
    `((h3 (@ (align "center")) "Welcome to "  ,(a 'nomad "Nomad"))
      (p "Nomad is a " ,(a 'emacs "Emacs") " like web browser (and more) that consists of a modular feature-set, fully programmable in "
         ,(a 'guile "Guile Scheme") ".")
      (h4 "Web Mode Keymap")
      ,(keymap->table (@ (nomad web) %web-mode-map))
      (h4 "Global Keymap")
      ,(keymap->table global-map))))

(define-view (404-view)
  "Returns HTML string with 404 error."
  ;; FIXME: Don't use a new <web-buffer>. This is just a hack till we have
  ;; proper regex URI handling.
  (let* ((uri  (string->uri (current-url)))
         (prev (current-buffer))
         (path (uri-path uri)))
    (if (file-exists? path)
        (begin (with-buffer prev
                 (kill-buffer))
               (make-buffer <web-buffer>
                            #:uri (string-append "file://" path)))
        (begin (rename-buffer (current-buffer) "404")
               '(h1 "404 view not found ")))))

(define-view (info-view)
  "TODO:"
  `((h2 (@ (align "center")) "Info")))

(define %nomad-restful-views `(("" . ("Welcome" . ,root-view))))
