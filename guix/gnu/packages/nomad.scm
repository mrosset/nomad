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

(define-public shroud-0.1.2
  (package (inherit shroud)
    (name "shroud")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/"
                                  name "/shroud-" version ".tar.gz"))
              (sha256
               (base32
                "1l2shrhvcwfzkar9qiwb75nhcqmx25iz55lzmz0c187nbjhqzi9p"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-shroud
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (guile (assoc-ref inputs "guile"))
                    (xclip (assoc-ref inputs "xclip"))
                    (prog (string-append out "/bin/shroud"))
                    (deps (list xclip))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            (string-append guile "/bin/guile")
                                            "-c" "(display (effective-version))")))
                    (path (map (cut string-append <> "/bin")
                               (delete #f deps)))
                    (ccachedir (string-append out
                                              "/lib/guile/" effective "/site-ccache")))
               (wrap-program prog
                 `("PATH" ":" prefix ,path)
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,ccachedir)))
               #t))))))
    (inputs
     `(("guile" ,guile-2.2)
       ("gnupg" ,gnupg)
       ("xclip" ,xclip)))))

(define-public guile-g-golf
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
        (name "guile-g-golf")
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
        (build-system glib-or-gtk-build-system)
        (native-inputs
         `(("autoconf" ,autoconf)
           ("automake" ,automake)
           ("texinfo" ,texinfo-6.6)
           ("gettext" ,gettext-minimal)
           ("libtool" ,libtool)
           ("pkg-config" ,pkg-config)))
        (inputs
         `(("guile" ,guile-2.2)
           ("guile-lib" ,guile-lib)
           ("glib" ,glib)
           ("gobject-introspection" ,gobject-introspection)))
        (home-page "https://www.gnu.org/software/g-golf/")
        (synopsis "G-Golf is a Guile Object Library for GNOME")
        (description "G-Golf low level API comprises a binding to - (most of) the
GObject Introspection and (some of) the GObject and Glib libraries, as well as
additional (G-Golf) utilities - used to import GObject libraries and build
their corresponding G-Golf high level API.")
        (license license:lgpl3+)))))

(define-public nomad
  (let ((commit "1a1bb61048cfc34b83d388e38b5612d369a2b9df"))
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
                  "1a4am9ak98y0p3ibiji14lr99lqplis08lka2kz0zyqalh4y8c68"))))
      (build-system gnu-build-system)
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
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-g-golf" ,guile-g-golf)
         ("guile-readline" ,guile-readline)
         ("gnutls" ,gnutls)
         ("shroud" ,shroud-0.1.2)
         ;; waiting on shroud to be updated in guix
         ("emacsy" ,emacsy-git)
         ;; emacsy needs to be updated in guix
         ("glib" ,glib)
         ("dbus-glib" ,dbus-glib)
         ("gtk+" ,gtk+)
         ("gtksourceview" ,gtksourceview)
         ("webkitgtk" ,webkitgtk)
         ("xorg-server" ,xorg-server)))
      (propagated-inputs
       `(("glib-networking" ,glib-networking)
         ("gsettings-desktop-schemas"
          ,gsettings-desktop-schemas)))
      (arguments
       `(#:tests? #t
         #:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (add-before 'check 'start-xorg-server
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; The test suite requires a running X server.
                       (system (format #f "~a/bin/Xvfb :1 &"
                                       (assoc-ref inputs "xorg-server")))
                       (setenv "DISPLAY" ":1")
                       #t))
           (add-after 'install 'wrap-binaries
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (gio-deps (map (cut assoc-ref inputs <>) '("glib-networking" "glib")))
                      (gio-mod-path (map (cut string-append <> "/lib/gio/modules") gio-deps))
                      (effective (read-line (open-pipe* OPEN_READ
                                                        "guile" "-c"
                                                        "(display (effective-version))")))
                      (deps (map (cut assoc-ref inputs <>) '("emacsy" "guile-lib"
                                                             "guile-readline" "shroud" "guile-g-golf")))
                      (scm-path (map (cut string-append <>
                                          "/share/guile/site/" effective)
                                     `(,out ,@deps)))
                      (go-path (map (cut string-append <>
                                         "/lib/guile/" effective "/site-ccache")
                                    `(,out ,@deps)))
                      (progs (map (cut string-append out "/bin/" <>)
                                  '("nomad"))))
                 (map (cut wrap-program <>
                           `("GIO_EXTRA_MODULES" ":" prefix ,gio-mod-path)
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
