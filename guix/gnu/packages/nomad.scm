(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix gexp)
 (guix download)
 (guix build-system gnu)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages autotools)
 (gnu packages glib)
 (gnu packages gnome)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages guile)
 (gnu packages password-utils)
 (gnu packages pkg-config)
 (gnu packages tls)
 (gnu packages webkit))

(define-public emacsy-git
  (let ((commit "f6334add3d51521acd5fcdd539a45ec2e5423530"))
    (package (inherit emacsy)
	     (name "emacsy-git")
	     (version (git-version "0.4.1" "5" commit))
	     (source (origin (method git-fetch)
			     (uri (git-reference (url "https://git.savannah.gnu.org/git/emacsy.git")
						 (commit commit)))
			     (file-name (string-append name "-" version))
			     (sha256 (base32 "0y1lx3xjp0xswgsw5lnnijgqfjwpjfixm5aaj70kw3yw1cixcfy6")))))))

(define-public nomad
  (let ((commit "161950216e0b355c2e47f3484805bde25a6a9542"))
    (package
      (name "nomad")
      (version (git-version "0.0.4-alpha" "375" commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://git.savannah.gnu.org/git/nomad.git")
		      (commit commit)))
		(file-name (git-file-name name version))
		(sha256
		 (base32
		  "15v8glkagzdag75dgqxs697n3vn8sxlrgs2kyjr6s6hiy9l62lsd"))))
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
	 ("webkitgtk" ,webkitgtk)))
      (propagated-inputs
       `(("dbus-glib" ,dbus-glib)
	 ("shroud" ,shroud)
	 ("emacsy" ,emacsy-git)
	 ("glib-networking" ,glib-networking)
	 ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
	 ("guile" ,guile-2.2)
	 ("guile-readline" ,guile-readline)))
      (arguments `(#:tests? #f))
      (home-page "https://savannah.nongnu.org/projects/nomad/")
      (synopsis "An extensible web browser using GNU Guile")
      (description "An extensible web browser.")
      (license license:gpl3+))))
