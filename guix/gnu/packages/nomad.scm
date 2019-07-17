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
 (gnu packages glib)
 (gnu packages gnome)
 (gnu packages gnupg)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages guile)
 (gnu packages password-utils)
 (gnu packages pkg-config)
 (gnu packages tls)
 (gnu packages webkit)
 (gnu packages xdisorg))

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

(define-public nomad
  (let ((commit "d3ecd7486b7f65f1bbd52e4251d77354d81707f3"))
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
                  "0lbqr1rm94w341vyv7nvpxmcl4arlac36b2k7z40af89d451m78k"))))
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
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-readline" ,guile-readline)
         ("gnutls" ,gnutls)
         ("shroud" ,shroud-0.1.2)
         ;; waiting on shroud to be updated in guix
         ("emacsy" ,emacsy-git)
         ;; emacsy needs to be updated in guix
         ("glib" ,glib)
         ("dbus-glib" ,dbus-glib)
         ("glib-networking" ,glib-networking)
         ("gsettings-desktop-schemas"
          ,gsettings-desktop-schemas)
         ("gtk+" ,gtk+)
         ("gtksourceview" ,gtksourceview)
         ("webkitgtk" ,webkitgtk)))
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
                      (deps (map (cut assoc-ref inputs <>) '("emacsy" "guile-lib"
                                                             "guile-readline" "shroud")))
                      (scm-path (map (cut string-append <>
                                          "/share/guile/site/" effective)
                                     `(,out ,@deps)))
                      (go-path (map (cut string-append <>
                                         "/lib/guile/" effective "/site-ccache")
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
