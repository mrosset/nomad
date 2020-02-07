(define-module (gnu packages nomad))

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
       ("webkitgtk" ,webkitgtk-unstable)
       ("g-golf" ,g-golf)
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
            (files '("lib/girepository-1.0")))))
    (home-page "https://savannah.nongnu.org/projects/nomad/")
    (synopsis "Web Browser extensible in Guile scheme")
    (description "Nomad is a Emacs-like web browser that consists of a small C
backend and modular feature-set fully programmable in Guile.")
    (license license:gpl3+)))

(define-public webkitgtk-unstable
  (package
    (inherit webkitgtk)
    (name "webkitgtk-unstable")
    (version "2.27.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  "webkitgtk-" version ".tar.xz"))
              (sha256
               (base32
                "1xvl93ajqgkar6ij15966mdzq1fijispgjafah60g4s98r2hlmah"))))))

(define-public nomad-git
  (let ((commit "37295da03aa1b0fc27b27048dbb5887214e9f13d"))
    (package
      (inherit nomad)
      (name "nomad-git")
      (version (git-version "1" "873" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/nomad.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mdd065di5sq8lwh5h0azxwsxdklzbp0idj07pqgahalxmj8y4jr")))))))
