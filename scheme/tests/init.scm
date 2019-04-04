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

(define-module (tests util)
  #:use-module (nomad init)
  #:use-module (srfi srfi-64))

(define home (getenv "HOME"))
(define home-dir (string-append home file-name-separator-string))
(define data "data/")
(define nomad.d "data/nomad.d")

(when (not (file-exists? data))
  (mkdir data #o755))

(test-begin "init")


(test-assert "data exists" (file-is-directory? test-data))


(with-fluid* user-nomad-directory nomad.d
	     (lambda ()
	       (test-equal "user-nomad-directory" "data/nomad.d"
			   (fluid-ref user-nomad-directory))))


(test-equal "user-init-file expands to $HOME/.nomad"
  user-init-file (string-append home-dir ".nomad"))

(test-equal "user-nomad-directory expands to $HOME/nomad.d"
  (fluid-ref user-nomad-directory) (string-append home-dir ".nomad.d"))


(with-fluid* user-nomad-directory nomad.d
	     (lambda ()
	       (create-nomad-directory)
	       (test-assert "user dir exists"
			      (file-is-directory? (fluid-ref user-nomad-directory)))
	       (rmdir (fluid-ref user-nomad-directory))))

(test-end)
