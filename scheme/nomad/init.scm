;; init.scm
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

(define-module (nomad init)
  #:use-module (nomad keymap)
  #:use-module (nomad events)
  #:use-module (nomad util)
  #:export (init
            user-cookie-file
            user-init-file
            user-init-hook
            user-nomad-directory
            create-nomad-directory))

(define user-init-hook (make-hook))

(define user-init-file
  (~/ ".nomad"))

(define user-nomad-directory
  (make-fluid (~/ ".nomad.d")))

(define user-cookie-file
  (string-append (fluid-ref user-nomad-directory) // "cookies.db"))

(define (create-nomad-directory)
  (let ((dir (fluid-ref user-nomad-directory)))
    (when (not (file-exists? dir))
      (info (format #f "creating ~a" dir))
      (mkdir dir #o755))))

(define (init)
  (add-hook! key-press-hook handle-key-press)
  (add-hook! event-hook debug-event)
  (create-nomad-directory)
  (if (file-exists? user-init-file)
      (load user-init-file)))
