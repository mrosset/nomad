(define-module (gnu packages g-golf))

(use-modules
 (guix packages)
 (guix git-download)
 (guix gexp)
 (guix download)
 (guix build-system gnu)
 (guix build-system glib-or-gtk)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (gnu packages autotools)
 (gnu packages curl)
 (gnu packages gettext)
 (gnu packages glib)
 (gnu packages gnome)
 (gnu packages gnupg)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages guile)
 (gnu packages password-utils)
 (gnu packages pkg-config)
 (gnu packages tls)
 (gnu packages texinfo)
 (gnu packages webkit)
 (gnu packages xdisorg)
 (gnu packages xorg)
)

(define-public g-golf
  (let ((texinfo-6.6
         (package (inherit texinfo)
                  (name "texinfo")
                  (version "6.6")
                  (source (origin
                            (method url-fetch)
                            (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                                version ".tar.xz"))
                            (sha256
                             (base32
                              "0rixv4c301djr0d0cnsxs8c1wjndi6bf9vi5axz6mwjkv80cmfcv"))))
                  (native-inputs '()))))

    (let ((commit "722b6d6ae9dcb70d584860e1331dfc0aa8a2ba12"))
      (package
        (name "g-golf")
        (version (git-version "1" "1" commit))
        (source (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://git.savannah.gnu.org/git/g-golf.git")
                        (commit commit)))
                  (file-name (git-file-name name version))
                  (sha256
                   (base32
                    "06lwgjq4a9bxsznjigqj5rb5h467ijyxgyxhva4bwmx4l84l5whh"))))
        (build-system gnu-build-system)
        (native-inputs
         `(("autoconf" ,autoconf)
           ("automake" ,automake)
           ("texinfo" ,texinfo-6.6)
           ("gettext" ,gettext-minimal)
           ("libtool" ,libtool)
           ("clutter" ,clutter) ;; probably want to remove this clutter?
           ;; FIXME: clutter is only required for tests
           ("pkg-config" ,pkg-config)))
        (inputs
         `(("guile" ,guile-2.2)
           ("guile-lib" ,guile-lib)
           ("glib" ,glib)
           ("gobject-introspection" ,gobject-introspection)))
        (arguments
         `(#:tests? #t
           #:modules ((ice-9 popen)
                    (ice-9 rdelim)
                    ,@%gnu-build-system-modules)
           #:phases
           (modify-phases %standard-phases
             (add-before 'configure 'setenv
               (lambda _
                 (setenv "GUILE_AUTO_COMPILE" "0")
                 #t))
              (add-before 'build 'substitute-libs
                (lambda* (#:key inputs #:allow-other-keys)
                  (let* ((get (lambda* (x #:optional (path ""))
                                (string-append (assoc-ref inputs x) "/lib/" path)))
                         (gi (get "gobject-introspection" "libgirepository-1.0.so"))
                         (glib (get "glib" "libglib-2.0.so"))
                         (gobject (get "glib" "libgobject-2.0.so")))
                    (substitute* "g-golf/init.scm"
                                (("libgirepository-1.0") gi)
                                (("libglib-2.0") glib)
                                (("libgobject-2.0") gobject))
                    #t)))
              (add-after 'install 'install-guile-modules
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (guile (assoc-ref inputs "guile"))
                         (effective (read-line (open-pipe* OPEN_READ
                                                           "guile" "-c"
                                                           "(display (effective-version))")))
                         (go-path (string-append out "/lib/guile/" effective "/site-ccache"))
                         (scm-path (string-append out "/share/guile/site/" effective)))
                    (mkdir-p go-path)
                    (mkdir-p scm-path)
                    (copy-recursively (string-append out
                                                     "/lib/g-golf/guile/" effective "/site-ccache")
                                      go-path)
                    (copy-recursively (string-append out
                                                     "/share/g-golf")
                                      scm-path)))))))
        (home-page "https://www.gnu.org/software/g-golf/")
        (synopsis "G-Golf is a Guile Object Library for GNOME")
        (description "G-Golf low level API comprises a binding to - (most of) the
GObject Introspection and (some of) the GObject and Glib libraries, as well as
additional (G-Golf) utilities - used to import GObject libraries and build
their corresponding G-Golf high level API.")
        (license license:lgpl3+)))))
