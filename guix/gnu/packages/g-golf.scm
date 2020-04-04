(define-module (gnu packages g-golf))

(use-modules ((guix licenses) #:prefix license:)
             (guix utils)
             (gnu packages autotools)
             (gnu packages gettext)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages gtk)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (guix build-system gnu)
             (guix download)
             (guix git-download)
             (guix packages))

(define-public g-golf
  (let ((commit "4a4edf25e4877df9182c77843bdd98ab59e13ef7"))
    (package
      (name "g-golf")
      (version (git-version "1" "683" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/g-golf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09p0gf71wbmlm9kri693a8fvr9hl3hhlmlidyadwjdh7853xg0h8"))))
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
         ("clutter" ,clutter)
         ("gtk" ,gtk+)
         ("glib" ,glib)))
      (propagated-inputs
       `(("gobject-introspection" ,gobject-introspection)))
      (arguments
       `(#:tests? #t
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'tests-work-arounds
             (lambda* (#:key inputs #:allow-other-keys)
               ;; In build environment, There is no /dev/tty
               (substitute*
                   "test-suite/tests/gobject.scm"
                 (("/dev/tty") "/dev/null"))))
           (add-before 'configure 'substitute-libs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((get (lambda (key lib)
                             (string-append (assoc-ref inputs key) "/lib/" lib)))
                      (libgi      (get "gobject-introspection" "libgirepository-1.0"))
                      (libglib    (get "glib" "libglib-2.0"))
                      (libgobject (get "glib" "libgobject-2.0"))
                      (libgdk     (get "gtk" "libgdk-3")))
                 (substitute* "configure"
                   (("SITEDIR=\"\\$datadir/g-golf\"")
                    "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
                   (("SITECCACHEDIR=\"\\$libdir/g-golf/")
                    "SITECCACHEDIR=\"$libdir/"))
                 (substitute* "g-golf/init.scm"
                   (("libgirepository-1.0") libgi)
                   (("libglib-2.0") libglib)
                   (("libgdk-3") libgdk)
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
