(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix gexp)
 (guix download)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (gnu packages autotools)
 (gnu packages bash)
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
 (gnu packages texinfo)
 (gnu packages perl))

(define-public nomad
  (package
    (name "nomad")
    (version "v0.1.2-candidate")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/nomad.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h0gjzl6h9zzifqghdxzv7zir3gwh024821ppc3ajsm8gnjzq4a7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash" ,bash)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("guile" ,guile-2.2)
       ("glib:bin" ,glib "bin")
       ("texinfo" ,texinfo)
       ("perl" ,perl)))
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile-lib)
       ("guile-gcrypt" ,guile-gcrypt)
       ("guile-readline" ,guile-readline)
       ("gnutls" ,gnutls)
       ("shroud" ,shroud)
       ("emacsy" ,emacsy-minimal)
       ("glib" ,glib)
       ("dbus-glib" ,dbus-glib)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("webkitgtk" ,webkitgtk)
       ("xorg-server" ,xorg-server)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
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
                    (gio-deps (map (cut assoc-ref inputs <>) '("glib-networking"
                                                               "glib")))
                    (gio-mod-path (map (cut string-append <> "/lib/gio/modules")
                                       gio-deps))
                    (effective (read-line (open-pipe*
                                           OPEN_READ
                                           "guile" "-c"
                                           "(display (effective-version))")))
                    (deps (map (cut assoc-ref inputs <>)
                               '("emacsy" "guile-lib" "guile-readline"
                                 "shroud")))
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
                         ;; FIXME: effective might fail
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
