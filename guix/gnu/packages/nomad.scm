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
  (let ((commit "0b93645c4ec4479ab775b9ea25b0e9b645dca633"))
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
		  "1g1jyvwdmq17p0i9qavrk45caicfsmcwm51hcbshdg5mci2lvqqp"))))
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
