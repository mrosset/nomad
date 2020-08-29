(define-module (gnu packages nomad)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages video)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages guile-xyz)
  #:export (cairo-gl
            libnice-webrtc
            gst-plugins-base-webrtc
            gst-plugins-bad-webrtc
            gstreamer-webrtc
            webkitgtk-webrtc
            emacsy-git
            g-golf-git
            nomad-git))

(define cairo-gl
  (let ((parent cairo))
    (package
      (inherit parent)
      (name "cairo-gl")
      (inputs (cons* `("mesa" ,mesa)
                   (package-inputs cairo))))))

(define libnice-webrtc
  (let ((parent libnice))
    (package
      (inherit parent)
      (name "libnice-webrtc")
      (inputs (cons* `("gst-plugins-base" ,gst-plugins-base-webrtc)
                     `("gst-plugins-bad" ,gst-plugins-base-webrtc)
                     (package-inputs parent))))))

(define gst-plugins-base-webrtc
  (let ((parent gst-plugins-base))
    (package
      (inherit parent)
      (name "gst-plugins-base-webrtc")
      (version "1.17.90")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://gstreamer.freedesktop.org/src/"
                             (package-name parent) "/"
                             (package-name parent) "-" version ".tar.xz"))
         (sha256
          (base32
           "0rdvhynradj7xya1vpfnv7c8p5j0z7wqs43vz0js5jkcrvakiwgn"))))
      (arguments
       (substitute-keyword-arguments (package-arguments parent)
         ((#:configure-flags flags)
          `(cons* "-Dgl=enabled"
                  "-Ddoc=disabled"
                  (delete "-Ddoc=false" (delete "-Dgl=disabled" ,flags))))
         ((#:tests? _ #f) #f)))
      (inputs (cons* `("mesa" ,mesa)
                     `("gstreamer", gstreamer-webrtc)
                     (package-inputs parent))))))

(define gst-plugins-bad-webrtc
  (let ((parent gst-plugins-bad))
    (package
      (inherit parent)
      (name "gst-plugins-bad-webrtc")
      (version "1.17.90")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://gstreamer.freedesktop.org/src/"
                             (package-name parent) "/"
                             (package-name parent) "-" version ".tar.xz"))
         (sha256
          (base32
           "1xmi60gxzhyswsm729ad8bhypscnlj6m0psxhp9dksn64ck7gv4b"))))
      (arguments
       (substitute-keyword-arguments (package-arguments parent)
         ((#:configure-flags flags)
          `(cons* "-Dwebrtc=enabled"
                  "-Dwebrtcdsp=enabled" ,flags))
         ((#:tests? _ #f) #f)))
      (inputs (cons* `("gnutls" ,gnutls)
                     `("libnice" ,libnice-webrtc)
                     `("gstreamer" ,gstreamer-webrtc)
                     `("gst-plugins-base" ,gst-plugins-base-webrtc)
                     (delete "gst-plugins-base"(package-inputs parent)))))))

(define gstreamer-webrtc-broken
  (package
    (inherit gstreamer)
    (name "gstreamer-webrtc-broken")
    (version "1.17.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
            version ".tar.xz"))
      (sha256
       (base32
        "0yqgjz2k9az8d29zjbailipn9rs2ivdrb3iz9kq229h3gisnbv08"))))
    (arguments
       (substitute-keyword-arguments (package-arguments gstreamer)
         ((#:phases phases)
          `(modify-phases ,phases (delete 'move-docs)))))
    (inputs (cons* `("gtk-doc" ,gtk-doc)
                   (package-inputs gstreamer)))))

(define gstreamer-webrtc
  (package
    (name "gstreamer-webrtc")
    (version "1.17.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
            version ".tar.xz"))
      (sha256
       (base32
        "0yqgjz2k9az8d29zjbailipn9rs2ivdrb3iz9kq229h3gisnbv08"))))
    (build-system meson-build-system)
    (outputs '("out"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'increase-test-timeout
           (lambda _
             (substitute* "tests/check/meson.build"
               (("'CK_DEFAULT_TIMEOUT', '20'")
                "'CK_DEFAULT_TIMEOUT', '60'")
               (("timeout ?: 3 \\* 60")
                "timeout: 9 * 60"))
             #t))
         ;; FIXME: Since switching to the meson-build-system, two tests
         ;; started failing on i686.  See
         ;; <https://gitlab.freedesktop.org/gstreamer/gstreamer/issues/499>.
         ,@(if (string-prefix? "i686" (or (%current-target-system)
                                          (%current-system)))
               `((add-after 'unpack 'disable-some-tests
                   (lambda _
                     (substitute* "tests/check/gst/gstsystemclock.c"
                       (("tcase_add_test \\(tc_chain, test_stress_cleanup_unschedule.*")
                        "")
                       (("tcase_add_test \\(tc_chain, test_stress_reschedule.*")
                      ""))
                     #t)))
               '()))))
    (propagated-inputs `(("glib" ,glib))) ; required by gstreamer-1.0.pc.
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (native-search-paths
     (list (search-path-specification
            (variable "GST_PLUGIN_SYSTEM_PATH")
            (files '("lib/gstreamer-1.0")))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Multimedia library")
    (description
     "GStreamer is a library for constructing graphs of media-handling
components.  The applications it supports range from simple Ogg/Vorbis
playback, audio/video streaming to complex audio mixing and video
non-linear editing.

Applications can take advantage of advances in codec and filter technology
transparently.  Developers can add new codecs and filters by writing a
simple plugin with a clean, generic interface.

This package provides the core library and elements.")
    (license license:lgpl2.0+)))

(define webkitgtk-webrtc
  (let ((commit "5c6400b8853b2f0f5dcfc91d8e7c44a7acf93d61")
        (hash   (base32 "08y6zij6643qldv4h1m37vjf1rdhsnwkgb064izzv35zblgskkk7")))
    (package
      (inherit webkitgtk)
      (name "webkitgtk-webrtc")
      (version (git-version "2.29.4" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/philn/webkit")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash)))
      (arguments
       (substitute-keyword-arguments (package-arguments webkitgtk)
         ((#:configure-flags original-configure-flags)
          `(cons* "-DENABLE_MINIBROWSER=ON"
                  "-DUSE_SYSTEMD=OFF"
                  "-DENABLE_GPU_PROCESS=OFF"
                  "-DENABLE_EXPERIMENTAL_FEATURES=ON"
                  ;; "-DENABLE_ACCELERATED_2D_CANVAS=ON"
                  "-DUSE_GSTREAMER_GL=OFF"
                  "-DUSE_GSTREAMER_WEBRTC=ON"
                  (delete "-DUSE_GSTREAMER_GL=OFF"
                          ,original-configure-flags)))))
      (inputs (cons* `("gtk+" ,gtk+)
                     ;; `("cairo" ,cairo-gl)
                     `("gstreamer" ,gstreamer-webrtc)
                     `("gst-plugins-bad" ,gst-plugins-bad-webrtc)
                     `("gst-plugins-base" ,gst-plugins-base-webrtc)
                     `("alsa-lib" ,alsa-lib)
                     `("libjpeg" ,libjpeg-turbo)
                     `("libvpx" ,libvpx)
                     `("libevent" ,libevent)
                     `("libopusenc" ,libopusenc)
                     (alist-delete "libjpeg" (package-inputs webkitgtk)))))))

(define emacsy-git
  (let ((commit "v0.4.1-30-g34032ff")
        (hash   (base32 "18kblii8cmk999k159lpdqhc1hc48ppag43kqz2j5nssm5y6smqq")))
    (package
      (inherit emacsy-minimal)
      (name "emacsy-git")
      (version (string-drop commit 1))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacsy.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash))))))

(define g-golf-git
  (let ((commit   "84e894eb7945c3bcdf7f8d5135c1be3efa524c92")
        (revision "822")
        (hash     (base32 "1pkcij65zy2lkip5yrfzj85nq17pp9mrf0d4sk6hpjqr4kd0bxd5")))
     (package
      (inherit g-golf)
      (name "g-golf-git")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/g-golf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash))))))

(define-public nomad-git
  (let ((commit "0.2.0-alpha-95-gbac1d3f")
        (hash   (base32 "1l5l2q2bs7fm7fgycsjrj6qwzivxm2lpwjwnwnyybvs3fy6i9h3d")))
    (package
      (inherit parent)
      (name "nomad-git")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/nomad.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 hash)))
      (native-inputs (cons `("gettext" ,gnu-gettext)
                           (package-native-inputs parent)))
      (inputs (cons* `("g-golf" ,g-golf-git)
                     `("emacsy" ,emacsy-git)
                     `("webkitgtk-webrtc" ,webkitgtk-webrtc)
                     `("gstreamer" ,gstreamer-webrtc)
                     (let ((inputs (package-inputs parent))
                           (del      alist-delete))
                       (del "gstreamer" (del "webkitgtk" (del "gst-plugins-base" (del "webkitgtk" inputs)))))))
      (propagated-inputs (cons* `("webkitgtk-webrtc" ,webkitgtk-webrtc)
                                `("gst-plugins-base" ,gst-plugins-base-webrtc)
                                `("gst-plugins-bad" ,gst-plugins-bad-webrtc)
                                (let ((inputs (package-propagated-inputs parent))
                                      (del    alist-delete))
                                  (del "gstreamer" (del "gst-plugins-bad" (del "gst-plugins-base" (del "webkitgtk" inputs))))))))))
