(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix gexp)
 (guix download)
 (guix build-system gnu)
 (guix build-system glib-or-gtk)
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
  (let ((commit "ed88cfbe57d5a40ea4e1604bfdc61f10ff750626"))
    (package (inherit emacsy)
	     (name "emacsy-git")
	     (version (git-version "0.4.1" "5" commit))
	     (source (origin (method git-fetch)
			     (uri (git-reference (url "https://git.savannah.gnu.org/git/emacsy.git")
						 (commit commit)))
			     (file-name (string-append name "-" version))
			     (sha256 (base32 "05zgpdh997q53042w192xdzgnfv6ymmkb16xkgd0ssj5pnnccj28")))))))

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
      (build-system glib-or-gtk-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("libtool" ,libtool)
         ("guile" ,guile-2.2)
         ("glib:bin" ,glib "bin")))
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-lib" ,guile-lib)
         ("guile-readline" ,guile-readline)
         ("shroud" ,shroud)
         ("emacsy" ,emacsy-git)
         ("glib" ,glib)
         ("gtk+" ,gtk+)
         ("gtksourceview" ,gtksourceview)
         ("webkitgtk" ,webkitgtk)))
      (propagated-inputs
       `(("dbus-glib" ,dbus-glib)
         ("glib-networking" ,glib-networking)
         ("gsettings-desktop-schemas"
          ,gsettings-desktop-schemas)))
      (arguments
       `(#:tests? #t
         #:modules ((guix build gnu-build-system)
                    (guix build glib-or-gtk-build-system)
                    (guix build utils)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-binaries
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (effective (read-line (open-pipe* OPEN_READ
                                                        "guile" "-c"
                                                        "(display (effective-version))")))
                      (deps (map (cut assoc-ref inputs <>) '("emacsy")))
                      (scm-path (map (cut string-append <>
                                          "/share/guile/site/" effective)
                                     `(,out ,@deps)))
                      (go-path (map (cut string-append <>
                                         "/lib/guile/" effective "/site-ccache/")
                                    `(,out ,@deps)))
                      (progs (map (cut string-append out "/bin/" <>)
                                  '("nomad"))))
                 (map (cut wrap-program <>
                           `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                           `("GUILE_LOAD_COMPILED_PATH" ":"
                             prefix ,go-path))
                      progs)
                 #t))))))
      (home-page "https://savannah.nongnu.org/projects/nomad/")
      (synopsis "Web Browser extensible in Guile scheme")
      (description "Nomad is a Emacs-like web browser that consists of a small
C backend and modular feature-set fully programmable in Guile.")
      (license license:gpl3+))))
