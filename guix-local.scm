;; guix-local.scm
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


(add-to-load-path (string-append
                   (dirname (current-filename))
                   "/guix"))

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             ((gnu packages nomad) #:prefix nomad:)
             (gnu packages guile-xyz))

(define %source-dir (dirname (current-filename)))

(define nomad-local
  (package (inherit nomad:nomad)
           (name "nomad")
           (version "git")
           (source (local-file %source-dir
                               #:recursive? #t
                               #:select? (git-predicate %source-dir)))))

nomad-local
