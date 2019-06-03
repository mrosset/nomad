(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages pkg-config)
 (gnu packages glib)
 (gnu packages autotools)
 (gnu packages gtk)
 (gnu packages guile)
 (gnu packages gnome)
 (gnu packages webkit))

(define-public nomad
  ;; feature-qt branch
  (let ((commit "b372b3b984dc241760b578e8e3b40b5be6e2a0f2"))
    (package
      (name "nomad")
      (version (git-version "0.0.4-alpha" "118" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://github.com/mrosset/nomad")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "144kr50w9kdlgamv3rjny70f7hk40cfq5f0n0fws4lls56x3w41a"))))
      (build-system gnu-build-system)
      (native-inputs
       `(
	 ("libtool" ,libtool)
	 ("glib:bin" ,glib "bin")
	 ))
      (inputs
       `(
	 ("pkg-config" ,pkg-config)
	 ("gtksourceview" ,gtksourceview)
	 ("glib" ,glib)
	 ("glib-networking" ,glib-networking)
	 ("gtk" ,gtk+)
	 ("autoconf" ,autoconf)
	 ("automake" ,automake)
	 ("vte" ,vte)
	 ("webkitgtk" ,webkitgtk)
	 ))
      (propagated-inputs
       `(("guile" ,guile-2.2)))
      (home-page "https://savannah.nongnu.org/projects/nomad/")
      (synopsis "An extensible web browser using GNU Guile")
      (description "An extensible web browser.")
      (license license:gpl3+))))

nomad
