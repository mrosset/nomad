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
  (let ((commit "0e109d36013253571f57a0c8cfc27a33615b0da5"))
    (package
      (name "nomad")
      (version (git-version "0.0.4-alpha" "118" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://git.savannah.gnu.org/git/nomad.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "1pl4xf9p6hhwnh4rgb1if86fw2ax7dh8dd7hqjafk5p26mgwbwhx"))))
      (build-system gnu-build-system)
      (native-inputs
       `(
	 ("libtool" ,libtool)
	 ("glib:bin" ,glib "bin")
	 ))
      (inputs
       `(
	 ("autoconf" ,autoconf)
	 ("automake" ,automake)
	 ("glib" ,glib)
	 ("glib-networking" ,glib-networking)
	 ("gtk" ,gtk+)
	 ("gtksourceview" ,gtksourceview)
	 ("guile" ,guile-2.2)
	 ("guile-readline" ,guile-readline)
	 ("pkg-config" ,pkg-config)
	 ("vte" ,vte)
	 ("webkitgtk" ,webkitgtk)
	 ))
      (propagated-inputs
       `(("guile" ,guile-2.2)
	 ("gesettings-desktop-schemas" ,gsettings-desktop-schemas)
	 ("glib-networking" ,glib-networking)))
      (home-page "https://savannah.nongnu.org/projects/nomad/")
      (synopsis "An extensible web browser using GNU Guile")
      (description "An extensible web browser.")
      (license license:gpl3+))))

nomad
