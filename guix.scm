(use-modules
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system gnu)
 (gnu packages qt)
 (gnu packages pkg-config)
 (gnu packages guile)
 (gnu packages glib)
 (gnu packages gettext)
 (gnu packages autotools)
 ((guix licenses)
  #:prefix license:)
 (guix utils)
 (gnu packages cups)
 (gnu packages icu4c)
 (gnu packages re2c)
 (gnu packages bison)
 (gnu packages flex)
 (gnu packages gperf)
 (gnu packages pciutils)
 (gnu packages ruby)
 (gnu packages ninja)
 (gnu packages nss)
 )

(define-public qt-with-web-engine
  (package
    (inherit qt)
    (name "qt-with-web-engine")
    (version "5.11.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://download.qt.io/official_releases/qt/"
                (version-major+minor version)
                "/" version
                "/single/qt-everywhere-src-"
                version ".tar.xz"))
              (sha256
               (base32
                "0kgzy32s1fr22fxxfhcyncfryb3qxrznlr737r4y5khk4xj1g545"))))))

(define-public qtwebengine
  (package (inherit qtsvg)
    (name "qtwebengine")
    (version "5.12.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://download.qt.io/official_releases/qt/"
                       (substring version 0 4)
                       "/" version "/submodules/"
                       (string-append name "-everywhere-src-" version)
                       ".tar.xz"))
       (sha256
        (base32
         "14ylx8s7pck2gx0ki9dksqh8cs6rcz63lly83f463gn1c1p1saq8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(
       ("perl" ,perl)
       ("python-2" ,python-2)
       ("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)
       ("ruby" ,ruby)
       ("ninja" ,ninja)
       ))
    (inputs
     `(
       ;; qtbase
       ("qtbase" ,qtbase)

       ;; Missing module: qml quick
       ("qtdeclarative" ,qtdeclarative)

       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ;; only if more xcb-* depends are required, add them
       ("libxkbcommon" ,libxkbcommon)
       ("libx11" ,libx11)
       ("libxrender" ,libxrender)
       ("libxi" ,libxi)
       ;; OpenGL
       ("mesa" ,mesa)
       ;; Qt Quick 2; missing

       ;; Accessibility
       ;; ("python-atspi" ,python-atspi)
       ;; ("dbus" ,dbus)

       ;; Qt webkit; Optional?
       ;; ("qtwebkit" ,qtwebkit)

       ;; qt web engine
       ("libgcrypt" ,libgcrypt)
       ("pciutils" ,pciutils)
       ("nss" ,nss)
       ("libxtst" ,libxtst)
       ("gperf" ,gperf)
       ("cups-minimal" ,cups-minimal)
       ("pulseaudio" ,pulseaudio)
       ("libgudev" ,libgudev)
       ;; systemd-devel? no systemd on guix
       ("libcap" ,libcap)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("libxrandr" ,libxrandr)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("fontconfig" ,fontconfig)
       ("atk" ,atk)
       ;; qt multimedia
       ("qtmultimedia" ,qtmultimedia)
       ;; qdoc documentation generator tool

       ;; warning: A compatible version of re2c (>= 0.11.3) was not
       ;; found; changes to src/*.in.cc will not affect your build.
       ("re2c" ,re2c)
       ))
    (home-page "https://www.qt.io")
    (synopsis "Qt5WebEngine")
    (description "Qt5WebEngine for nomad. Provides support for web
applications using the Chromium browser project.")
    (license
     (package-license qt))))

(define-public nomad
  (package
    (name "nomad")
    (version "34e6c58977cfe472b96442b4de07a6caf189f74d")
    ;; feature-qt branch
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mrosset/nomad")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1prs0ws8ybfm65mh6mvnzlgfpfp2vfqrlcckkzqk95zxbks86kl5"))))
    (build-system gnu-build-system)
    (inputs
     `(
       ("pkg-config" ,pkg-config)
       ("glib" ,glib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("guile-2.2" ,guile-2.2)
       ("qtbase" ,qtbase)
       ("qtwebengine" ,qtwebengine)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qttools" ,qttools) ;; qtbase5-dev-tools

       ;; Current Status: "./configure" phase: Error
       ;; checking for Qt5Widgets... yes
       ;; checking for Qt5WebEngine... no
       ;; configure: error: Package requirements (Qt5WebEngine) were not met:
       ;; No package 'Qt5WebEngine' found
       
       ;; ("qtquickcontrols" ,qtquickcontrols)
       ;; ("qtwebengine5",qtwebengine5) ;; possibly missing from guix?
       ;; ("qml-module-qtquick2",qml-module-qtquick2)
       ;; maybe qtquickcontrols also
       ;; ("qml-module-qtwebengine",qml-module-qtwebengine)
       ;; ("qml-module-qtquick-layouts",qml-module-qtquick-layouts)
       ;; ("libqtermwidget5-0",libqtermwidget5-0)
       ("qtwayland" ,qtwayland) ;;?? ^libqtermwidget
       ))
    (home-page "https://github.com/mrosset/nomad")
    (synopsis "An extensible web browser using Gnu Guile and QT.")
    (description "An extensible web browser.")
    (license license:gpl3+)))

nomad
