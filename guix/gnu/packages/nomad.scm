(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages autotools)
 (gnu packages glib)
 (gnu packages gnome)
 (gnu packages gtk)
 (gnu packages guile)
 (gnu packages password-utils)
 (gnu packages pkg-config)
 (gnu packages tls)
 (gnu packages webkit))

(define-public nomad
  (let ((commit "3cb53d410c17c680bba3c62b0c5bf3f23140bafe"))
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
		  "0sbl5p5740zgzi3m8md1i1wqmrnpn89kz27k51wlan9rxbq7xasy"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("libtool" ,libtool)
	 ("glib:bin" ,glib "bin")))
      (inputs
       `(("autoconf" ,autoconf)
	 ("automake" ,automake)
	 ("glib" ,glib)
	 ("gtk+" ,gtk+)
	 ("gtksourceview" ,gtksourceview)
	 ("pkg-config" ,pkg-config)
	 ("vte" ,vte)
	 ("webkitgtk" ,webkitgtk)))
      (propagated-inputs
       `(("dbus-glib" ,dbus-glib)
         ("shroud" ,shroud)
	 ("glib-networking" ,glib-networking)
	 ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
	 ("guile" ,guile-2.2)
	 ("guile-readline" ,guile-readline)))
      (home-page "https://savannah.nongnu.org/projects/nomad/")
      (synopsis "An extensible web browser using GNU Guile")
      (description "An extensible web browser.")
      (license license:gpl3+))))
