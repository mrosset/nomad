(define-module (gnu packages g-golf))

(use-modules ((guix licenses) #:prefix license:)
             (guix utils)
             (gnu packages autotools)
             (gnu packages gettext)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (guix build-system gnu)
             (guix download)
             (guix git-download)
             (guix packages))

(define-public g-golf
  (let ((commit "d7bee600d7cf2cbfd0972d339f04fe4c74213848"))
    (package
      (name "g-golf")
      (version (git-version "1" "574" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/g-golf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c39iy1ckjwyiyrb9a9sll4yzp45322y9p0rb77ljns0lz3qsgll"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)
         ("gettext" ,gettext-minimal)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-lib" ,guile-lib)
         ("glib" ,glib)
         ("clutter" ,clutter)))
      (propagated-inputs
       `(("gobject-introspection" ,gobject-introspection)))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'substitute-libs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((get (lambda (key lib)
                             (string-append (assoc-ref inputs key) "/lib/" lib)))
                      (libgi      (get "gobject-introspection" "libgirepository-1.0.so"))
                      (libglib    (get "glib" "libglib-2.0.so"))
                      (libgobject (get "glib" "libgobject-2.0.so")))
                 (substitute* "configure"
                   (("SITEDIR=\"\\$datadir/g-golf\"")
                    "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
                   (("SITECCACHEDIR=\"\\$libdir/g-golf/")
                    "SITECCACHEDIR=\"$libdir/"))
                 (substitute* "g-golf/init.scm"
                   (("libgirepository-1.0") libgi)
                   (("libglib-2.0") libglib)
                   (("libgobject-2.0") libgobject)
                   (("\\(dynamic-link \"libg-golf\"\\)")
                    (format #f "~s"
                            `(dynamic-link
                              (format #f "~alibg-golf"
                                      (if (getenv "GUILE_GGOLF_UNINSTALLED")
                                          ""
                                          ,(format #f "~a/lib/"
                                                   (assoc-ref outputs "out"))))))))
                 (setenv "GUILE_AUTO_COMPILE" "0")
                 (setenv "GUILE_GGOLF_UNINSTALLED" "1")
                 #t))))))
      (home-page "https://www.gnu.org/software/g-golf/")
      (synopsis "G-Golf is a Guile Object Library for GNOME")
      (description "G-Golf low level API comprises a binding to - (most of) the
GObject Introspection and (some of) the GObject and Glib libraries, as well as
additional (G-Golf) utilities - used to import GObject libraries and build
their corresponding G-Golf high level API.")
      (license license:lgpl3+))))
