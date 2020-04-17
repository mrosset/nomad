(define-module (gnu packages nomad))

(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 (guix build-system glib-or-gtk)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (gnu packages autotools)
 (gnu packages bash)
 (gnu packages code)
 (gnu packages curl)
 (gnu packages emacsy)
 (gnu packages gettext)
 (gnu packages glib)
 (gnu packages g-golf)
 (gnu packages gnome)
 (gnu packages gnupg)
 (gnu packages gtk)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages gstreamer)
 (gnu packages password-utils)
 (gnu packages perl)
 (gnu packages pkg-config)
 (gnu packages tls)
 (gnu packages texinfo)
 (gnu packages webkit)
 (gnu packages xdisorg)
 (gnu packages xorg))

(define-public nomad
  (package
    (name "nomad")
    (version "0.1.2-alpha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/nomad.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dnkr1hmvfkwgxd75dcf93pg39yfgawvdpzdhv991yhghv0qxc9h"))))
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
       ("lcov" ,lcov)
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
       ("gtk+:bin" ,gtk+ "bin")
       ("gtksourceview" ,gtksourceview)
       ("webkitgtk" ,webkitgtk)
       ("g-golf" ,g-golf-git)
       ("xorg-server" ,xorg-server)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-ugly" ,gst-plugins-ugly)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("vte" ,vte)
       ("webkitgtk" ,webkitgtk)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (srfi srfi-26))
       #:tests? #f
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
                                                               "glib"
                                                               "gstreamer"
                                                               "gst-plugins-base"
                                                               "gst-plugins-good"
                                                               "gst-plugins-bad"
                                                               "gst-plugins-ugly")))
                    (gio-mod-path (map (cut string-append <> "/lib/gio/modules")
                                       gio-deps))
                    (effective (read-line (open-pipe*
                                           OPEN_READ
                                           "guile" "-c"
                                           "(display (effective-version))")))
                    (deps (map (cut assoc-ref inputs <>)
                               '("emacsy" "guile-lib" "guile-readline" "g-golf"
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
    (native-search-paths
     (list (search-path-specification
            (variable "GI_TYPELIB_PATH")
            (separator ":")
            (files '("lib/girepository-1.0")))
           (search-path-specification
            (variable "NOMAD_WEB_EXTENSION_DIR")
            (separator ":")
            (files '("libexec/nomad")))))
    (home-page "https://savannah.nongnu.org/projects/nomad/")
    (synopsis "Web Browser extensible in Guile scheme")
    (description "Nomad is a Emacs-like web browser that consists of a small C
backend and modular feature-set fully programmable in Guile.")
    (license license:gpl3+)))

(define-public nomad-git
  (let ((commit "ee866b30bc1a408a4c5bc2a52c79df006c4e2fdc"))
    (package
      (inherit nomad)
      (name "nomad-git")
      (version (git-version (package-version nomad) "304" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/nomad.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a3rbiyfpd5519xad60w3kjy5b5kx96ywx81pcf5gqca1vsc18wj")))))))
