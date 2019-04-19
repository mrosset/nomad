(define-public guile-nomad
  (package
    (name "guile-nomad")
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
       ;; ("qtwayland" ,qtwayland)?? ^libqtermwidget
       ))
    (home-page "https://github.com/mrosset/nomad")
    (synopsis "An extensible web browser using Gnu Guile and QT.")
    (description "An extensible web browser.")
    (license license:gpl3+)))
