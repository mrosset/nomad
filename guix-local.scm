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

(load (string-append
		 (dirname (current-filename))
		 "/guix/gnu/packages/nomad.scm"))

(use-modules (guix packages)
	     (guix git-download)
	     (gnu packages nomad))

;; Override nomad's nomad sources uri to use guix-testing branch. This
;; allows for testing development builds of nomad without effecting
;; stable end user release build or git history.
(define nomad-local
  (let ((commit "0fb72953af7e1f202e6f1a1692fa1a4b784cb58e"))
    (package (inherit nomad)
	     (name "nomad")
	     (version (git-version "alpha" "testing" commit))
	     (source (origin
		       (method git-fetch)
		       (uri (git-reference
			     (url "https://git.savannah.gnu.org/git/nomad.git")
			     (commit commit)))
		       (file-name (git-file-name name version))
		       (sha256
			(base32
			 "1b06nb5mmj7xsqharfh4plfcbv4z2jylj06blgn53vs2dfjhmzhi")))))))

nomad-local
