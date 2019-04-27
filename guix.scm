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
       ("qt-with-web-engine" ,qt-with-web-engine)
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
